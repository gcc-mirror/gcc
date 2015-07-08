/* Offload image generation tool for PTX.

   Copyright (C) 2014-2015 Free Software Foundation, Inc.

   Contributed by Nathan Sidwell <nathan@codesourcery.com> and
   Bernd Schmidt <bernds@codesourcery.com>.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Munges PTX assembly into a C source file defining the PTX code as a
   string.

   This is not a complete assembler.  We presume the source is well
   formed from the compiler and can die horribly if it is not.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "intl.h"
#include <libgen.h>
#include "obstack.h"
#include "diagnostic.h"
#include "collect-utils.h"
#include "gomp-constants.h"

const char tool_name[] = "nvptx mkoffload";

#define COMMENT_PREFIX "#"

typedef enum Kind
{
  /* 0-ff used for single char tokens */
  K_symbol = 0x100, /* a symbol */
  K_label,  /* a label defn (i.e. symbol:) */
  K_ident,  /* other ident */
  K_dotted, /* dotted identifier */
  K_number,
  K_string,
  K_comment
} Kind;

typedef struct Token
{
  unsigned short kind : 12;
  unsigned short space : 1; /* preceded by space */
  unsigned short end : 1;   /* succeeded by end of line */
  /* Length of token */
  unsigned short len;

  /* Token itself */
  char const *ptr;
} Token;

/* statement info */
typedef enum Vis
{
  V_dot = 0,  /* random pseudo */
  V_var = 1,  /* var decl/defn */
  V_func = 2, /* func decl/defn */
  V_insn = 3, /* random insn */
  V_label = 4, /* label defn */
  V_comment = 5,
  V_pred = 6,  /* predicate */
  V_mask = 0x7,
  V_global = 0x08, /* globalize */
  V_weak = 0x10,   /* weakly globalize */
  V_no_eol = 0x20, /* no end of line */
  V_prefix_comment = 0x40 /* prefixed comment */
} Vis;

typedef struct Stmt
{
  struct Stmt *next;
  Token *tokens;
  unsigned char vis;
  unsigned len : 12;
  unsigned sym : 12;
} Stmt;

struct id_map
{
  id_map *next;
  char *ptx_name;
};

static const char *read_file (FILE *);
static Token *tokenize (const char *);

static void write_token (FILE *, const Token *);
static void write_tokens (FILE *, const Token *, unsigned, int);

static Stmt *alloc_stmt (unsigned, Token *, Token *, const Token *);
#define alloc_comment(S,E) alloc_stmt (V_comment, S, E, 0)
#define append_stmt(V, S) ((S)->next = *(V), *(V) = (S))
static Stmt *rev_stmts (Stmt *);
static void write_stmt (FILE *, const Stmt *);
static void write_stmts (FILE *, const Stmt *);

static Token *parse_insn (Token *);
static Token *parse_list_nosemi (Token *);
static Token *parse_init (Token *);
static Token *parse_file (Token *);

static Stmt *decls;
static Stmt *vars;
static Stmt *fns;

static id_map *func_ids, **funcs_tail = &func_ids;
static id_map *var_ids, **vars_tail = &var_ids;

/* Files to unlink.  */
static const char *ptx_name;
static const char *ptx_cfile_name;

/* Shows if we should compile binaries for i386 instead of x86-64.  */
bool target_ilp32 = false;

/* Delete tempfiles.  */

/* Unlink a temporary file unless requested otherwise.  */

void
maybe_unlink (const char *file)
{
  if (! debug)
    {
      if (unlink_if_ordinary (file)
	  && errno != ENOENT)
	fatal_error (input_location, "deleting file %s: %m", file);
    }
  else
    fprintf (stderr, "[Leaving %s]\n", file);
}

void
tool_cleanup (bool)
{
}

/* Add or change the value of an environment variable, outputting the
   change to standard error if in verbose mode.  */
static void
xputenv (const char *string)
{
  if (verbose)
    fprintf (stderr, "%s\n", string);
  putenv (CONST_CAST (char *, string));
}


static void
record_id (const char *p1, id_map ***where)
{
  const char *end = strchr (p1, '\n');
  if (!end)
    fatal_error (input_location, "malformed ptx file");

  id_map *v = XNEW (id_map);
  size_t len = end - p1;
  v->ptx_name = XNEWVEC (char, len + 1);
  memcpy (v->ptx_name, p1, len);
  v->ptx_name[len] = '\0';
  v->next = NULL;
  id_map **tail = *where;
  *tail = v;
  *where = &v->next;
}

/* Read the whole input file.  It will be NUL terminated (but
   remember, there could be a NUL in the file itself.  */

static const char *
read_file (FILE *stream)
{
  size_t alloc = 16384;
  size_t base = 0;
  char *buffer;

  if (!fseek (stream, 0, SEEK_END))
    {
      /* Get the file size.  */
      long s = ftell (stream);
      if (s >= 0)
	alloc = s + 100;
      fseek (stream, 0, SEEK_SET);
    }
  buffer = XNEWVEC (char, alloc);

  for (;;)
    {
      size_t n = fread (buffer + base, 1, alloc - base - 1, stream);

      if (!n)
	break;
      base += n;
      if (base + 1 == alloc)
	{
	  alloc *= 2;
	  buffer = XRESIZEVEC (char, buffer, alloc);
	}
    }
  buffer[base] = 0;
  return buffer;
}

/* Read a token, advancing ptr.
   If we read a comment, append it to the comments block. */

static Token *
tokenize (const char *ptr)
{
  unsigned alloc = 1000;
  unsigned num = 0;
  Token *toks = XNEWVEC (Token, alloc);
  int in_comment = 0;
  int not_comment = 0;

  for (;; num++)
    {
      const char *base;
      unsigned kind;
      int ws = 0;
      int eol = 0;

    again:
      base = ptr;
      if (in_comment)
	goto block_comment;
      switch (kind = *ptr++)
	{
	default:
	  break;

	case '\n':
	  eol = 1;
	  /* Fall through */
	case ' ':
	case '\t':
	case '\r':
	case '\v':
	  /* White space */
	  ws = not_comment;
	  goto again;

	case '/':
	  {
	    if (*ptr == '/')
	      {
		/* line comment.  Do not include trailing \n */
		base += 2;
		for (; *ptr; ptr++)
		  if (*ptr == '\n')
		    break;
		kind = K_comment;
	      }
	    else if (*ptr == '*')
	      {
		/* block comment */
		base += 2;
		ptr++;

	      block_comment:
		eol = in_comment;
		in_comment = 1;
		for (; *ptr; ptr++)
		  {
		    if (*ptr == '\n')
		      {
			ptr++;
			break;
		      }
		    if (ptr[0] == '*' && ptr[1] == '/')
		      {
			in_comment = 2;
			ptr += 2;
			break;
		      }
		  }
		kind = K_comment;
	      }
	    else
	      break;
	  }
	  break;

	case '"':
	  /* quoted string */
	  kind = K_string;
	  while (*ptr)
	    if (*ptr == '"')
	      {
		ptr++;
		break;
	      }
	    else if (*ptr++ == '\\')
	      ptr++;
	  break;

	case '.':
	  if (*ptr < '0' || *ptr > '9')
	    {
	      kind = K_dotted;
	      ws = not_comment;
	      goto ident;
	    }
	  /* FALLTHROUGH */
	case '0'...'9':
	  kind = K_number;
	  goto ident;
	  break;

	case '$':  /* local labels.  */
	case '%':  /* register names, pseudoes etc */
	  kind = K_ident;
	  goto ident;

	case 'a'...'z':
	case 'A'...'Z':
	case '_':
	  kind = K_symbol; /* possible symbol name */
	ident:
	  for (; *ptr; ptr++)
	    {
	      if (*ptr >= 'A' && *ptr <= 'Z')
		continue;
	      if (*ptr >= 'a' && *ptr <= 'z')
		continue;
	      if (*ptr >= '0' && *ptr <= '9')
		continue;
	      if (*ptr == '_' || *ptr == '$')
		continue;
	      if (*ptr == '.' && kind != K_dotted)
		/* Idents starting with a dot, cannot have internal dots. */
		continue;
	      if ((*ptr == '+' || *ptr == '-')
		  && kind == K_number
		  && (ptr[-1] == 'e' || ptr[-1] == 'E'
		      || ptr[-1] == 'p' || ptr[-1] == 'P'))
		/* exponent */
		continue;
	      break;
	    }
	  if (*ptr == ':')
	    {
	      ptr++;
	      kind = K_label;
	    }
	  break;
	}

      if (alloc == num)
	{
	  alloc *= 2;
	  toks = XRESIZEVEC (Token, toks, alloc);
	}
      Token *tok = toks + num;

      tok->kind = kind;
      tok->space = ws;
      tok->end = 0;
      tok->ptr = base;
      tok->len = ptr - base - in_comment;
      in_comment &= 1;
      not_comment = kind != K_comment;
      if (eol && num)
	tok[-1].end = 1;
      if (!kind)
	break;
    }

  return toks;
}

/* Write an encoded token. */

static void
write_token (FILE *out, Token const *tok)
{
  if (tok->space)
    fputc (' ', out);

  switch (tok->kind)
    {
    case K_string:
      {
	const char *c = tok->ptr + 1;
	size_t len = tok->len - 2;

	fputs ("\\\"", out);
	while (len)
	  {
	    const char *bs = (const char *)memchr (c, '\\', len);
	    size_t l = bs ? bs - c : len;

	    fprintf (out, "%.*s", (int)l, c);
	    len -= l;
	    c += l;
	    if (bs)
	      {
		fputs ("\\\\", out);
		len--, c++;
	      }
	  }
	fputs ("\\\"", out);
      }
      break;

    default:
      /* All other tokens shouldn't have anything magic in them */
      fprintf (out, "%.*s", tok->len, tok->ptr);
      break;
    }
  if (tok->end)
    fputs ("\\n", out);
}

static void
write_tokens (FILE *out, Token const *toks, unsigned len, int spc)
{
  fputs ("\t\"", out);
  for (; len--; toks++)
    write_token (out, toks);
  if (spc)
    fputs (" ", out);
  fputs ("\"", out);
}

static Stmt *
alloc_stmt (unsigned vis, Token *tokens, Token *end, Token const *sym)
{
  static unsigned alloc = 0;
  static Stmt *heap = 0;

  if (!alloc)
    {
      alloc = 1000;
      heap = XNEWVEC (Stmt, alloc);
    }

  Stmt *stmt = heap++;
  alloc--;

  tokens->space = 0;
  stmt->next = 0;
  stmt->vis = vis;
  stmt->tokens = tokens;
  stmt->len = end - tokens;
  stmt->sym = sym ? sym - tokens : ~0;

  return stmt;
}

static Stmt *
rev_stmts (Stmt *stmt)
{
  Stmt *prev = 0;
  Stmt *next;

  while (stmt)
    {
      next = stmt->next;
      stmt->next = prev;
      prev = stmt;
      stmt = next;
    }

  return prev;
}

static void
write_stmt (FILE *out, const Stmt *stmt)
{
  if ((stmt->vis & V_mask) != V_comment)
    {
      write_tokens (out, stmt->tokens, stmt->len,
		    (stmt->vis & V_mask) == V_pred);
      fputs (stmt->vis & V_no_eol ? "\t" : "\n", out);
    }
}

static void
write_stmts (FILE *out, const Stmt *stmts)
{
  for (; stmts; stmts = stmts->next)
    write_stmt (out, stmts);
}

static Token *
parse_insn (Token *tok)
{
  unsigned depth = 0;

  do
    {
      Stmt *stmt;
      Token *sym = 0;
      unsigned s = V_insn;
      Token *start = tok;

      switch (tok++->kind)
	{
	case K_comment:
	  while (tok->kind == K_comment)
	    tok++;
	  stmt = alloc_comment (start, tok);
	  append_stmt (&fns, stmt);
	  continue;

	case '{':
	  depth++;
	  break;

	case '}':
	  depth--;
	  break;

	case K_label:
	  if (tok[-1].ptr[0] != '$')
	    sym = tok - 1;
	  tok[-1].end = 1;
	  s = V_label;
	  break;

	case '@':
	  tok->space = 0;
	  if (tok->kind == '!')
	    tok++;
	  if (tok->kind == K_symbol)
	    sym = tok;
	  tok++;
	  s = V_pred;
	  break;

	default:
	  for (; tok->kind != ';'; tok++)
	    {
	      if (tok->kind == ',')
		tok[1].space = 0;
	      else if (tok->kind == K_symbol)
		sym = tok;
	    }
	  tok++->end = 1;
	  break;
	}

      stmt = alloc_stmt (s, start, tok, sym);
      append_stmt (&fns, stmt);

      if (!tok[-1].end && tok[0].kind == K_comment)
	{
	  stmt->vis |= V_no_eol;
	  stmt = alloc_comment (tok, tok + 1);
	  append_stmt (&fns, stmt);
	  tok++;
	}
    }
  while (depth);

  return tok;
}

/* comma separated list of tokens */

static Token *
parse_list_nosemi (Token *tok)
{
  Token *start = tok;

  do
    if (!(++tok)->kind)
      break;
  while ((++tok)->kind == ',');

  tok[-1].end = 1;
  Stmt *stmt = alloc_stmt (V_dot, start, tok, 0);
  append_stmt (&decls, stmt);

  return tok;
}

#define is_keyword(T,S) \
  (sizeof (S) == (T)->len && !memcmp ((T)->ptr + 1, (S), (T)->len - 1))

static Token *
parse_init (Token *tok)
{
  for (;;)
    {
      Token *start = tok;
      Token const *sym = 0;
      Stmt *stmt;

      if (tok->kind == K_comment)
	{
	  while (tok->kind == K_comment)
	    tok++;
	  stmt = alloc_comment (start, tok);
	  append_stmt (&vars, stmt);
	  start = tok;
	}

      if (tok->kind == '{')
	tok[1].space = 0;
      for (; tok->kind != ',' && tok->kind != ';'; tok++)
	if (tok->kind == K_symbol)
	  sym = tok;
      tok[1].space = 0;
      int end = tok++->kind == ';';
      stmt = alloc_stmt (V_insn, start, tok, sym);
      append_stmt (&vars, stmt);
      if (!tok[-1].end && tok->kind == K_comment)
	{
	  stmt->vis |= V_no_eol;
	  stmt = alloc_comment (tok, tok + 1);
	  append_stmt (&vars, stmt);
	  tok++;
	}
      if (end)
	break;
    }
  return tok;
}

static Token *
parse_file (Token *tok)
{
  Stmt *comment = 0;

  if (tok->kind == K_comment)
    {
      Token *start = tok;

      while (tok->kind == K_comment)
	{
	  if (strncmp (tok->ptr, ":VAR_MAP ", 9) == 0)
	    record_id (tok->ptr + 9, &vars_tail);
	  if (strncmp (tok->ptr, ":FUNC_MAP ", 10) == 0)
	    record_id (tok->ptr + 10, &funcs_tail);
	  tok++;
	}
      comment = alloc_comment (start, tok);
      comment->vis |= V_prefix_comment;
    }

  if (tok->kind == K_dotted)
    {
      if (is_keyword (tok, "version")
	  || is_keyword (tok, "target")
	  || is_keyword (tok, "address_size"))
	{
	  if (comment)
	    append_stmt (&decls, comment);
	  tok = parse_list_nosemi (tok);
	}
      else
	{
	  unsigned vis = 0;
	  const Token *def = 0;
	  unsigned is_decl = 0;
	  Token *start;

	  for (start = tok;
	       tok->kind && tok->kind != '=' && tok->kind != K_comment
		 && tok->kind != '{' && tok->kind != ';'; tok++)
	    {
	      if (is_keyword (tok, "global")
		  || is_keyword (tok, "const"))
		vis |= V_var;
	      else if (is_keyword (tok, "func")
		       || is_keyword (tok, "entry"))
		vis |= V_func;
	      else if (is_keyword (tok, "visible"))
		vis |= V_global;
	      else if (is_keyword (tok, "extern"))
		is_decl = 1;
	      else if (is_keyword (tok, "weak"))
		vis |= V_weak;
	      if (tok->kind == '(')
		{
		  tok[1].space = 0;
		  tok[0].space = 1;
		}
	      else if (tok->kind == ')' && tok[1].kind != ';')
		tok[1].space = 1;

	      if (tok->kind == K_symbol)
		def = tok;
	    }

	  if (!tok->kind)
	    {
	      /* end of file */
	      if (comment)
		append_stmt (&fns, comment);
	    }
	  else if (tok->kind == '{'
		   || tok->kind == K_comment)
	    {
	      /* function defn */
	      Stmt *stmt = alloc_stmt (vis, start, tok, def);
	      if (comment)
		{
		  append_stmt (&fns, comment);
		  stmt->vis |= V_prefix_comment;
		}
	      append_stmt (&fns, stmt);
	      tok = parse_insn (tok);
	    }
	  else
	    {
	      int assign = tok->kind == '=';

	      tok++->end = 1;
	      if ((vis & V_mask) == V_var && !is_decl)
		{
		  /* variable */
		  Stmt *stmt = alloc_stmt (vis, start, tok, def);
		  if (comment)
		    {
		      append_stmt (&vars, comment);
		      stmt->vis |= V_prefix_comment;
		    }
		  append_stmt (&vars, stmt);
		  if (assign)
		    tok = parse_init (tok);
		}
	      else
		{
		  /* declaration */
		  Stmt *stmt = alloc_stmt (vis, start, tok, 0);
		  if (comment)
		    {
		      append_stmt (&decls, comment);
		      stmt->vis |= V_prefix_comment;
		    }
		  append_stmt (&decls, stmt);
		}
	    }
	}
    }
  else
    {
      /* Something strange.  Ignore it.  */
      if (comment)
	append_stmt (&fns, comment);

      do
	tok++;
      while (tok->kind && !tok->end);
    }
  return tok;
}

/* Parse STR, saving found tokens into PVALUES and return their number.
   Tokens are assumed to be delimited by ':'.  */
static unsigned
parse_env_var (const char *str, char ***pvalues)
{
  const char *curval, *nextval;
  char **values;
  unsigned num = 1, i;

  curval = strchr (str, ':');
  while (curval)
    {
      num++;
      curval = strchr (curval + 1, ':');
    }

  values = (char **) xmalloc (num * sizeof (char *));
  curval = str;
  nextval = strchr (curval, ':');
  if (nextval == NULL)
    nextval = strchr (curval, '\0');

  for (i = 0; i < num; i++)
    {
      int l = nextval - curval;
      values[i] = (char *) xmalloc (l + 1);
      memcpy (values[i], curval, l);
      values[i][l] = 0;
      curval = nextval + 1;
      nextval = strchr (curval, ':');
      if (nextval == NULL)
	nextval = strchr (curval, '\0');
    }
  *pvalues = values;
  return num;
}

/* Auxiliary function that frees elements of PTR and PTR itself.
   N is number of elements to be freed.  If PTR is NULL, nothing is freed.
   If an element is NULL, subsequent elements are not freed.  */
static void
free_array_of_ptrs (void **ptr, unsigned n)
{
  unsigned i;
  if (!ptr)
    return;
  for (i = 0; i < n; i++)
    {
      if (!ptr[i])
	break;
      free (ptr[i]);
    }
  free (ptr);
  return;
}

/* Check whether NAME can be accessed in MODE.  This is like access,
   except that it never considers directories to be executable.  */
static int
access_check (const char *name, int mode)
{
  if (mode == X_OK)
    {
      struct stat st;

      if (stat (name, &st) < 0 || S_ISDIR (st.st_mode))
	return -1;
    }

  return access (name, mode);
}

static void
process (FILE *in, FILE *out)
{
  const char *input = read_file (in);
  Token *tok = tokenize (input);
  unsigned int nvars = 0, nfuncs = 0;

  do
    tok = parse_file (tok);
  while (tok->kind);

  fprintf (out, "static const char ptx_code[] = \n");
  write_stmts (out, rev_stmts (decls));
  write_stmts (out, rev_stmts (vars));
  write_stmts (out, rev_stmts (fns));
  fprintf (out, ";\n\n");
  fprintf (out, "static const char *var_mappings[] = {\n");
  for (id_map *id = var_ids; id; id = id->next, nvars++)
    fprintf (out, "\t\"%s\"%s\n", id->ptx_name, id->next ? "," : "");
  fprintf (out, "};\n\n");
  fprintf (out, "static const char *func_mappings[] = {\n");
  for (id_map *id = func_ids; id; id = id->next, nfuncs++)
    fprintf (out, "\t\"%s\"%s\n", id->ptx_name, id->next ? "," : "");
  fprintf (out, "};\n\n");

  fprintf (out, "static const void *target_data[] = {\n");
  fprintf (out, "  ptx_code, (void*) %u, var_mappings, (void*) %u, "
		"func_mappings\n", nvars, nfuncs);
  fprintf (out, "};\n\n");

  fprintf (out, "extern void GOMP_offload_register (const void *, int, void *);\n");

  fprintf (out, "extern void *__OFFLOAD_TABLE__[];\n\n");
  fprintf (out, "static __attribute__((constructor)) void init (void)\n{\n");
  fprintf (out, "  GOMP_offload_register (__OFFLOAD_TABLE__, %d,\n",
	   GOMP_DEVICE_NVIDIA_PTX);
  fprintf (out, "                         &target_data);\n");
  fprintf (out, "};\n");
}

static void
compile_native (const char *infile, const char *outfile, const char *compiler)
{
  const char *collect_gcc_options = getenv ("COLLECT_GCC_OPTIONS");
  if (!collect_gcc_options)
    fatal_error (input_location,
		 "environment variable COLLECT_GCC_OPTIONS must be set");

  struct obstack argv_obstack;
  obstack_init (&argv_obstack);
  obstack_ptr_grow (&argv_obstack, compiler);
  obstack_ptr_grow (&argv_obstack, target_ilp32 ? "-m32" : "-m64");
  obstack_ptr_grow (&argv_obstack, infile);
  obstack_ptr_grow (&argv_obstack, "-c");
  obstack_ptr_grow (&argv_obstack, "-o");
  obstack_ptr_grow (&argv_obstack, outfile);
  obstack_ptr_grow (&argv_obstack, NULL);

  const char **new_argv = XOBFINISH (&argv_obstack, const char **);
  fork_execute (new_argv[0], CONST_CAST (char **, new_argv), true);
  obstack_free (&argv_obstack, NULL);
}

int
main (int argc, char **argv)
{
  FILE *in = stdin;
  FILE *out = stdout;
  const char *outname = 0;

  progname = "mkoffload";
  diagnostic_initialize (global_dc, 0);

  char *collect_gcc = getenv ("COLLECT_GCC");
  if (collect_gcc == NULL)
    fatal_error (input_location, "COLLECT_GCC must be set.");
  const char *gcc_path = dirname (ASTRDUP (collect_gcc));
  const char *gcc_exec = basename (ASTRDUP (collect_gcc));

  size_t len = (strlen (gcc_path) + 1
		+ strlen (GCC_INSTALL_NAME)
		+ 1);
  char *driver = XALLOCAVEC (char, len);

  if (strcmp (gcc_exec, collect_gcc) == 0)
    /* collect_gcc has no path, so it was found in PATH.  Make sure we also
       find accel-gcc in PATH.  */
    gcc_path = NULL;

  int driver_used = 0;
  if (gcc_path != NULL)
    driver_used = sprintf (driver, "%s/", gcc_path);
  sprintf (driver + driver_used, "%s", GCC_INSTALL_NAME);

  bool found = false;
  if (gcc_path == NULL)
    found = true;
  else if (access_check (driver, X_OK) == 0)
    found = true;
  else
    {
      /* Don't use alloca pointer with XRESIZEVEC.  */
      driver = NULL;
      /* Look in all COMPILER_PATHs for GCC_INSTALL_NAME.  */
      char **paths = NULL;
      unsigned n_paths;
      n_paths = parse_env_var (getenv ("COMPILER_PATH"), &paths);
      for (unsigned i = 0; i < n_paths; i++)
	{
	  len = strlen (paths[i]) + 1 + strlen (GCC_INSTALL_NAME) + 1;
	  driver = XRESIZEVEC (char, driver, len);
	  sprintf (driver, "%s/%s", paths[i], GCC_INSTALL_NAME);
	  if (access_check (driver, X_OK) == 0)
	    {
	      found = true;
	      break;
	    }
	}
      free_array_of_ptrs ((void **) paths, n_paths);
    }

  if (!found)
    fatal_error (input_location,
		 "offload compiler %s not found", GCC_INSTALL_NAME);

  /* We may be called with all the arguments stored in some file and
     passed with @file.  Expand them into argv before processing.  */
  expandargv (&argc, &argv);

  /* Find out whether we should compile binaries for i386 or x86-64.  */
  for (int i = argc - 1; i > 0; i--)
    if (strncmp (argv[i], "-foffload-abi=", sizeof ("-foffload-abi=") - 1) == 0)
      {
	if (strstr (argv[i], "ilp32"))
	  target_ilp32 = true;
	else if (!strstr (argv[i], "lp64"))
	  fatal_error (input_location,
		       "unrecognizable argument of option -foffload-abi");
	break;
      }

  struct obstack argv_obstack;
  obstack_init (&argv_obstack);
  obstack_ptr_grow (&argv_obstack, driver);
  obstack_ptr_grow (&argv_obstack, "-xlto");
  obstack_ptr_grow (&argv_obstack, target_ilp32 ? "-m32" : "-m64");
  obstack_ptr_grow (&argv_obstack, "-S");

  for (int ix = 1; ix != argc; ix++)
    {
      if (!strcmp (argv[ix], "-o") && ix + 1 != argc)
	outname = argv[++ix];
      else
	obstack_ptr_grow (&argv_obstack, argv[ix]);
    }

  ptx_cfile_name = make_temp_file (".c");

  out = fopen (ptx_cfile_name, "w");
  if (!out)
    fatal_error (input_location, "cannot open '%s'", ptx_cfile_name);

  /* PR libgomp/65099: Currently, we only support offloading in 64-bit
     configurations.  */
  if (!target_ilp32)
    {
      ptx_name = make_temp_file (".mkoffload");
      obstack_ptr_grow (&argv_obstack, "-o");
      obstack_ptr_grow (&argv_obstack, ptx_name);
      obstack_ptr_grow (&argv_obstack, NULL);
      const char **new_argv = XOBFINISH (&argv_obstack, const char **);

      char *execpath = getenv ("GCC_EXEC_PREFIX");
      char *cpath = getenv ("COMPILER_PATH");
      char *lpath = getenv ("LIBRARY_PATH");
      unsetenv ("GCC_EXEC_PREFIX");
      unsetenv ("COMPILER_PATH");
      unsetenv ("LIBRARY_PATH");

      fork_execute (new_argv[0], CONST_CAST (char **, new_argv), true);
      obstack_free (&argv_obstack, NULL);

      xputenv (concat ("GCC_EXEC_PREFIX=", execpath, NULL));
      xputenv (concat ("COMPILER_PATH=", cpath, NULL));
      xputenv (concat ("LIBRARY_PATH=", lpath, NULL));

      in = fopen (ptx_name, "r");
      if (!in)
	fatal_error (input_location, "cannot open intermediate ptx file");

      process (in, out);
    }

  fclose (out);

  compile_native (ptx_cfile_name, outname, collect_gcc);

  utils_cleanup (false);

  return 0;
}
