/* Preprocess only, using cpplib.
   Copyright (C) 1995-2024 Free Software Foundation, Inc.
   Written by Per Bothner, 1994-95.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "c-common.h"		/* For flags.  */
#include "../libcpp/internal.h"
#include "langhooks.h"
#include "c-pragma.h"		/* For parse_in.  */
#include "file-prefix-map.h"    /* remap_macro_filename()  */

class token_streamer;

/* Encapsulates state used to convert a stream of tokens into a text
   file.  */
static struct
{
  FILE *outf;			/* Stream to write to.  */
  const cpp_token *prev;	/* Previous token.  */
  const cpp_token *source;	/* Source token for spacing.  */
  unsigned src_line;		/* Line number currently being written.  */
  bool printed;			/* True if something output at line.  */
  bool first_time;		/* pp_file_change hasn't been called yet.  */
  bool prev_was_system_token;	/* True if the previous token was a
				   system token.*/
  const char *src_file;		/* Current source file.  */
  token_streamer *streamer;     /* Instance of class token_streamer using this
				   object.  */
} print;

/* Defined and undefined macros being queued for output with -dU at
   the next newline.  */
struct macro_queue
{
  struct macro_queue *next;	/* Next macro in the list.  */
  char *macro;			/* The name of the macro if not
				   defined, the full definition if
				   defined.  */
};
static macro_queue *define_queue, *undef_queue;

/* General output routines.  */
static void scan_translation_unit (cpp_reader *);
static void scan_translation_unit_directives_only (cpp_reader *);
static void scan_translation_unit_trad (cpp_reader *);
static void account_for_newlines (const unsigned char *, size_t);
static int dump_macro (cpp_reader *, cpp_hashnode *, void *);
static void dump_queued_macros (cpp_reader *);

static bool print_line_1 (location_t, const char*, FILE *);
static bool print_line (location_t, const char *);
static bool maybe_print_line_1 (location_t, FILE *);
static bool maybe_print_line (location_t);
static bool do_line_change (cpp_reader *, const cpp_token *,
			    location_t, int);

/* Callback routines for the parser.   Most of these are active only
   in specific modes.  */
static void cb_line_change (cpp_reader *, const cpp_token *, int);
static void cb_define (cpp_reader *, location_t, cpp_hashnode *);
static void cb_undef (cpp_reader *, location_t, cpp_hashnode *);
static void cb_used_define (cpp_reader *, location_t, cpp_hashnode *);
static void cb_used_undef (cpp_reader *, location_t, cpp_hashnode *);
static void cb_include (cpp_reader *, location_t, const unsigned char *,
			const char *, int, const cpp_token **);
static void cb_ident (cpp_reader *, location_t, const cpp_string *);
static void cb_def_pragma (cpp_reader *, location_t);
static void cb_read_pch (cpp_reader *pfile, const char *name,
			 int fd, const char *orig_name);

/* Preprocess and output.  */
void
preprocess_file (cpp_reader *pfile)
{
  /* A successful cpp_read_main_file guarantees that we can call
     cpp_scan_nooutput or cpp_get_token next.  */
  if (flag_no_output && pfile->buffer)
    {
      if (flag_modules)
	{
	  /* For macros from imported headers we need directives_only_cb.  */
	  cpp_get_options (pfile)->directives_only = true;
	  scan_translation_unit_directives_only (pfile);
	}
      else
	{
	  /* Scan -included buffers, then the main file.  */
	  while (pfile->buffer->prev)
	    cpp_scan_nooutput (pfile);
	  cpp_scan_nooutput (pfile);
	}
    }
  else if (cpp_get_options (pfile)->traditional)
    scan_translation_unit_trad (pfile);
  else if (cpp_get_options (pfile)->directives_only
	   && !cpp_get_options (pfile)->preprocessed)
    scan_translation_unit_directives_only (pfile);
  else
    scan_translation_unit (pfile);

  /* -dM command line option.  Should this be elsewhere?  */
  if (flag_dump_macros == 'M')
    cpp_forall_identifiers (pfile, dump_macro, NULL);

  /* Flush any pending output.  */
  if (print.printed)
    putc ('\n', print.outf);
}

/* Don't emit #pragma or #ident directives if we are processing
   assembly language; the assembler may choke on them.  */
static bool
should_output_pragmas ()
{
  return cpp_get_options (parse_in)->lang != CLK_ASM;
}

/* Set up the callbacks as appropriate.  */
void
init_pp_output (FILE *out_stream)
{
  cpp_callbacks *cb = cpp_get_callbacks (parse_in);

  if (!flag_no_output)
    {
      cb->line_change = cb_line_change;
      if (should_output_pragmas ())
	{
	  cb->ident      = cb_ident;
	  cb->def_pragma = cb_def_pragma;
	}
    }

  if (flag_dump_includes)
    cb->include  = cb_include;

  if (flag_pch_preprocess)
    {
      cb->valid_pch = c_common_valid_pch;
      cb->read_pch = cb_read_pch;
    }

  if (flag_dump_macros == 'N' || flag_dump_macros == 'D')
    {
      cb->define = cb_define;
      cb->undef  = cb_undef;
    }

  if (flag_dump_macros == 'U')
    {
      cb->before_define = dump_queued_macros;
      cb->used_define = cb_used_define;
      cb->used_undef = cb_used_undef;
    }

  cb->has_attribute = c_common_has_attribute;
  cb->has_builtin = c_common_has_builtin;
  cb->has_feature = c_common_has_feature;
  cb->get_source_date_epoch = cb_get_source_date_epoch;
  cb->get_suggestion = cb_get_suggestion;
  cb->remap_filename = remap_macro_filename;

  /* Initialize the print structure.  */
  print.src_line = 1;
  print.printed = false;
  print.prev = 0;
  print.outf = out_stream;
  print.first_time = 1;
  print.src_file = "";
  print.prev_was_system_token = false;
  print.streamer = nullptr;
}

// FIXME: Ideally we'd just turn the entirety of the print struct into
// an encapsulated streamer ...

class token_streamer
{
  bool avoid_paste;
  bool do_line_adjustments;
  bool in_pragma;

 public:
  token_streamer (cpp_reader *pfile)
    :avoid_paste (false),
    do_line_adjustments (cpp_get_options (pfile)->lang != CLK_ASM
			 && !flag_no_line_commands),
    in_pragma (false)
    {
      gcc_assert (!print.streamer);
      print.streamer = this;
    }

  void begin_pragma ()
  {
    in_pragma = true;
  }

  void stream (cpp_reader *pfile, const cpp_token *tok, location_t);
};

void
token_streamer::stream (cpp_reader *pfile, const cpp_token *token,
			location_t loc)
{
  /* Keep input_location up to date, since it is needed for processing early
     pragmas such as #pragma GCC diagnostic.  */
  input_location = loc;

  if (token->type == CPP_PADDING)
    {
      avoid_paste = true;
      if (print.source == NULL
	  || (!(print.source->flags & PREV_WHITE)
	      && token->val.source == NULL))
	print.source = token->val.source;
      return;
    }

  if (token->type == CPP_EOF)
    return;

  /* Keep track when we move into and out of system locations.  */
  const bool is_system_token = in_system_header_at (loc);
  const bool system_state_changed
    = (is_system_token != print.prev_was_system_token);
  print.prev_was_system_token = is_system_token;

  /* Subtle logic to output a space if and only if necessary.  */
  bool line_marker_emitted = false;
  if (avoid_paste)
    {
      unsigned src_line = LOCATION_LINE (loc);

      if (print.source == NULL)
	print.source = token;

      if (src_line != print.src_line
	  && do_line_adjustments
	  && !in_pragma)
	{
	  line_marker_emitted = do_line_change (pfile, token, loc, false);
	  putc (' ', print.outf);
	  print.printed = true;
	}
      else if (print.source->flags & PREV_WHITE
	       || (print.prev
		   && cpp_avoid_paste (pfile, print.prev, token))
	       || (print.prev == NULL && token->type == CPP_HASH))
	{
	  putc (' ', print.outf);
	  print.printed = true;
	}
    }
  else if (token->flags & PREV_WHITE && token->type != CPP_PRAGMA)
    {
      unsigned src_line = LOCATION_LINE (loc);

      if (src_line != print.src_line
	  && do_line_adjustments
	  && !in_pragma)
	line_marker_emitted = do_line_change (pfile, token, loc, false);
      putc (' ', print.outf);
      print.printed = true;
    }

  avoid_paste = false;
  print.source = NULL;
  print.prev = token;
  if (token->type == CPP_PRAGMA)
    {
      in_pragma = true;
      if (should_output_pragmas ())
	{
	  const char *space;
	  const char *name;

	  line_marker_emitted = maybe_print_line (token->src_loc);
	  fputs ("#pragma ", print.outf);
	  c_pp_lookup_pragma (token->val.pragma, &space, &name);
	  if (space)
	    fprintf (print.outf, "%s %s", space, name);
	  else
	    fprintf (print.outf, "%s", name);
	  print.printed = true;
	}
      if (token->val.pragma >= PRAGMA_FIRST_EXTERNAL)
	c_pp_invoke_early_pragma_handler (token->val.pragma);
    }
  else if (token->type == CPP_PRAGMA_EOL)
    {
      if (should_output_pragmas ())
	maybe_print_line (UNKNOWN_LOCATION);
      in_pragma = false;
    }
  else if (token->type == CPP_EMBED)
    {
      char buf[76 + 6];
      maybe_print_line (token->src_loc);
      gcc_checking_assert (token->val.str.len != 0);
      fputs ("#embed \".\" __gnu__::__base64__(", print.outf);
      if (token->val.str.len > 30)
	{
	  fputs (" \\\n", print.outf);
	  print.src_line++;
	}
      buf[0] = '"';
      memcpy (buf + 1 + 76, "\" \\\n", 5);
      unsigned int j = 1;
      static const char base64_enc[] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
      for (unsigned i = 0; ; i += 3)
	{
	  unsigned char a = token->val.str.text[i];
	  unsigned char b = 0, c = 0;
	  unsigned int n = token->val.str.len - i;
	  if (n > 1)
	    b = token->val.str.text[i + 1];
	  if (n > 2)
	    c = token->val.str.text[i + 2];
	  unsigned long v = ((((unsigned long) a) << 16)
			     | (((unsigned long) b) << 8)
			     | c);
	  buf[j++] = base64_enc[(v >> 18) & 63];
	  buf[j++] = base64_enc[(v >> 12) & 63];
	  buf[j++] = base64_enc[(v >> 6) & 63];
	  buf[j++] = base64_enc[v & 63];
	  if (j == 76 + 1 || n <= 3)
	    {
	      if (n < 3)
		{
		  buf[j - 1] = '=';
		  if (n == 1)
		    buf[j - 2] = '=';
		}
	      if (n <= 3)
		memcpy (buf + j, "\")", 3);
	      else
		print.src_line++;
	      fputs (buf, print.outf);
	      j = 1;
	      if (n <= 3)
		break;
	    }
	}
      print.printed = true;
      maybe_print_line (token->src_loc);
      return;
    }
  else
    {
      if (cpp_get_options (parse_in)->debug)
	linemap_dump_location (line_table, token->src_loc, print.outf);

      if (do_line_adjustments
	  && !in_pragma
	  && !line_marker_emitted
	  && system_state_changed
	  && !is_location_from_builtin_token (loc))
	/* The system-ness of this token is different from the one of
	   the previous token.  Let's emit a line change to mark the
	   new system-ness before we emit the token.  */
	{
	  line_marker_emitted = do_line_change (pfile, token, loc, false);
	}
      if (!in_pragma || should_output_pragmas ())
	{
	  cpp_output_token (token, print.outf);
	  print.printed = true;
	}
    }

  /* CPP_COMMENT tokens and raw-string literal tokens can have
     embedded new-line characters.  Rather than enumerating all the
     possible token types just check if token uses val.str union
     member.  */
  if (cpp_token_val_index (token) == CPP_TOKEN_FLD_STR)
    account_for_newlines (token->val.str.text, token->val.str.len);
}

/* Writes out the preprocessed file, handling spacing and paste
   avoidance issues.  */

static void
scan_translation_unit (cpp_reader *pfile)
{
  token_streamer streamer (pfile);
  uintptr_t filter = 0;

  if (lang_hooks.preprocess_token)
    filter = lang_hooks.preprocess_token (pfile, NULL, filter);

  print.source = NULL;
  for (;;)
    {
      location_t spelling_loc;
      const cpp_token *token
	= cpp_get_token_with_location (pfile, &spelling_loc);

      streamer.stream (pfile, token, spelling_loc);
      if (filter)
	{
	  unsigned flags = lang_hooks.preprocess_token (pfile, token, filter);
	  if (flags & lang_hooks::PT_begin_pragma)
	    streamer.begin_pragma ();
	}
      if (token->type == CPP_EOF)
	break;
    }

  if (filter)
    lang_hooks.preprocess_token (pfile, NULL, filter);
}

class do_streamer : public token_streamer
{
 public:
  uintptr_t filter;

  do_streamer (cpp_reader *pfile, uintptr_t filter)
    :token_streamer (pfile), filter (filter)
    {
    }
};

static void
directives_only_cb (cpp_reader *pfile, CPP_DO_task task, void *data_, ...)
{
  va_list args;
  va_start (args, data_);

  do_streamer *streamer = reinterpret_cast <do_streamer *> (data_);
  switch (task)
    {
    default:
      gcc_unreachable ();

    case CPP_DO_print:
      if (!flag_no_output)
	{
	  print.src_line += va_arg (args, unsigned);

	  const void *buf = va_arg (args, const void *);
	  size_t size = va_arg (args, size_t);
	  fwrite (buf, 1, size, print.outf);
	}
      break;

    case CPP_DO_location:
      if (!flag_no_output)
	maybe_print_line (va_arg (args, location_t));
      break;

    case CPP_DO_token:
      {
	const cpp_token *token = va_arg (args, const cpp_token *);
	unsigned flags = 0;
	if (streamer->filter)
	  flags = lang_hooks.preprocess_token (pfile, token, streamer->filter);
	if (!flag_no_output)
	  {
	    location_t spelling_loc = va_arg (args, location_t);
	    streamer->stream (pfile, token, spelling_loc);
	    if (flags & lang_hooks::PT_begin_pragma)
	      streamer->begin_pragma ();
	  }
      }
      break;
    }

  va_end (args);
}

/* Writes out the preprocessed file, handling spacing and paste
   avoidance issues.  */
static void
scan_translation_unit_directives_only (cpp_reader *pfile)
{
  uintptr_t filter = 0;
  if (lang_hooks.preprocess_token)
    filter = lang_hooks.preprocess_token (pfile, NULL, filter);
  do_streamer streamer (pfile, filter);
  cpp_directive_only_process (pfile, &streamer, directives_only_cb);
  if (streamer.filter)
    lang_hooks.preprocess_token (pfile, NULL, streamer.filter);
}

/* Adjust print.src_line for newlines embedded in output.  For example, if a raw
   string literal contains newlines, then we need to increment our notion of the
   current line to keep in sync and avoid outputting a line marker
   unnecessarily.  If a raw string literal containing newlines is the result of
   macro expansion, then we have the opposite problem, where the token takes up
   more lines in the output than it did in the input, and hence a line marker is
   needed to restore the correct state for subsequent lines.  In this case,
   incrementing print.src_line still does the job, because it will cause us to
   emit the line marker the next time a token is streamed.  */
static void
account_for_newlines (const unsigned char *str, size_t len)
{
  while (len--)
    if (*str++ == '\n')
      print.src_line++;
}

/* Writes out a traditionally preprocessed file.  */
static void
scan_translation_unit_trad (cpp_reader *pfile)
{
  while (_cpp_read_logical_line_trad (pfile))
    {
      size_t len = pfile->out.cur - pfile->out.base;
      maybe_print_line (pfile->out.first_line);
      fwrite (pfile->out.base, 1, len, print.outf);
      print.printed = true;
      if (!CPP_OPTION (pfile, discard_comments))
	account_for_newlines (pfile->out.base, len);
    }
}

/* If the token read on logical line LINE needs to be output on a
   different line to the current one, output the required newlines or
   a line marker.  If a line marker was emitted, return TRUE otherwise
   return FALSE.  */

static bool
maybe_print_line_1 (location_t src_loc, FILE *stream)
{
  bool emitted_line_marker = false;
  unsigned src_line = LOCATION_LINE (src_loc);
  const char *src_file = LOCATION_FILE (src_loc);

  /* End the previous line of text.  */
  if (print.printed)
    {
      putc ('\n', stream);
      print.src_line++;
      print.printed = false;
    }

  if (!flag_no_line_commands
      && src_line >= print.src_line
      && src_line < print.src_line + 8
      && src_loc != UNKNOWN_LOCATION
      && strcmp (src_file, print.src_file) == 0)
    {
      while (src_line > print.src_line)
	{
	  putc ('\n', stream);
	  print.src_line++;
	}
    }
  else
    emitted_line_marker = print_line_1 (src_loc, "", stream);

  return emitted_line_marker;
}

/* If the token read on logical line LINE needs to be output on a
   different line to the current one, output the required newlines or
   a line marker.  If a line marker was emitted, return TRUE otherwise
   return FALSE.  */

static bool
maybe_print_line (location_t src_loc)
{
  if (cpp_get_options (parse_in)->debug)
    linemap_dump_location (line_table, src_loc,
			   print.outf);
  return maybe_print_line_1 (src_loc, print.outf);
}

/* Output a line marker for logical line LINE.  Special flags are "1"
   or "2" indicating entering or leaving a file.  If the line marker
   was effectively emitted, return TRUE otherwise return FALSE.  */

static bool
print_line_1 (location_t src_loc, const char *special_flags, FILE *stream)
{
  bool emitted_line_marker = false;

  /* End any previous line of text.  */
  if (print.printed)
    putc ('\n', stream);
  print.printed = false;

  if (src_loc != UNKNOWN_LOCATION && !flag_no_line_commands)
    {
      const char *file_path = LOCATION_FILE (src_loc);
      size_t to_file_len = strlen (file_path);
      unsigned char *to_file_quoted =
         (unsigned char *) alloca (to_file_len * 4 + 1);

      /* cpp_quote_string does not nul-terminate, so we have to do it
	 ourselves.  */
      unsigned char *p = cpp_quote_string (to_file_quoted,
					   (const unsigned char *) file_path,
					   to_file_len);
      *p = '\0';

      print.src_line = LOCATION_LINE (src_loc);
      print.src_file = file_path;

      fprintf (stream, "# %u \"%s\"%s",
	       print.src_line, to_file_quoted, special_flags);

      int sysp = in_system_header_at (src_loc);
      if (sysp == 2)
	fputs (" 3 4", stream);
      else if (sysp == 1)
	fputs (" 3", stream);

      putc ('\n', stream);
      emitted_line_marker = true;
    }

  return emitted_line_marker;
}

/* Output a line marker for logical line LINE.  Special flags are "1"
   or "2" indicating entering or leaving a file.  Return TRUE if a
   line marker was effectively emitted, FALSE otherwise.  */

static bool
print_line (location_t src_loc, const char *special_flags)
{
    if (cpp_get_options (parse_in)->debug)
      linemap_dump_location (line_table, src_loc,
			     print.outf);
    return print_line_1 (src_loc, special_flags, print.outf);
}

/* Helper function for cb_line_change and scan_translation_unit.
   Return TRUE if a line marker is emitted, FALSE otherwise.  */
static bool
do_line_change (cpp_reader *pfile, const cpp_token *token,
		location_t src_loc, int parsing_args)
{
  bool emitted_line_marker = false;
  if (define_queue || undef_queue)
    dump_queued_macros (pfile);

  if (token->type == CPP_EOF || parsing_args)
    return false;

  emitted_line_marker = maybe_print_line (src_loc);
  print.prev = 0;
  print.source = 0;

  /* Supply enough spaces to put this token in its original column,
     one space per column greater than 2, since scan_translation_unit
     will provide a space if PREV_WHITE.  Don't bother trying to
     reconstruct tabs; we can't get it right in general, and nothing
     ought to care.  Some things do care; the fault lies with them.

     Also do not output the spaces if this is a CPP_PRAGMA token.  In this
     case, libcpp has provided the location of the first token after #pragma,
     so we would start at the wrong column.  */
  if (!CPP_OPTION (pfile, traditional) && token->type != CPP_PRAGMA)
    {
      int spaces = LOCATION_COLUMN (src_loc) - 2;
      print.printed = true;

      while (-- spaces >= 0)
	putc (' ', print.outf);
    }

  return emitted_line_marker;
}

/* Called when a line of output is started.  TOKEN is the first token
   of the line, and at end of file will be CPP_EOF.  */
static void
cb_line_change (cpp_reader *pfile, const cpp_token *token,
		int parsing_args)
{
  do_line_change (pfile, token, token->src_loc, parsing_args);
}

static void
cb_ident (cpp_reader *pfile ATTRIBUTE_UNUSED, location_t line,
	  const cpp_string *str)
{
  maybe_print_line (line);
  fprintf (print.outf, "#ident %s\n", str->text);
  print.src_line++;
}

static void
cb_define (cpp_reader *pfile, location_t line, cpp_hashnode *node)
{
  const line_map_ordinary *map;

  maybe_print_line (line);
  fputs ("#define ", print.outf);

  /* 'D' is whole definition; 'N' is name only.  */
  if (flag_dump_macros == 'D')
    fputs ((const char *) cpp_macro_definition (pfile, node),
	   print.outf);
  else
    fputs ((const char *) NODE_NAME (node), print.outf);

  putc ('\n', print.outf);
  print.printed = false;
  linemap_resolve_location (line_table, line,
			    LRK_MACRO_DEFINITION_LOCATION,
			    &map);
  print.src_line++;
}

static void
cb_undef (cpp_reader *pfile, location_t line, cpp_hashnode *node)
{
  if (lang_hooks.preprocess_undef)
    lang_hooks.preprocess_undef (pfile, line, node);
  maybe_print_line (line);
  fprintf (print.outf, "#undef %s\n", NODE_NAME (node));
  print.src_line++;
}

static void
cb_used_define (cpp_reader *pfile, location_t line ATTRIBUTE_UNUSED,
		cpp_hashnode *node)
{
  if (cpp_user_macro_p (node))
    {
      macro_queue *q;
      q = XNEW (macro_queue);
      q->macro = xstrdup ((const char *) cpp_macro_definition (pfile, node));
      q->next = define_queue;
      define_queue = q;
    }
}

static void
cb_used_undef (cpp_reader *pfile ATTRIBUTE_UNUSED,
	       location_t line ATTRIBUTE_UNUSED,
	       cpp_hashnode *node)
{
  macro_queue *q;
  q = XNEW (macro_queue);
  q->macro = xstrdup ((const char *) NODE_NAME (node));
  q->next = undef_queue;
  undef_queue = q;
}

static void
dump_queued_macros (cpp_reader *pfile ATTRIBUTE_UNUSED)
{
  macro_queue *q;

  /* End the previous line of text.  */
  if (print.printed)
    {
      putc ('\n', print.outf);
      print.src_line++;
      print.printed = false;
    }

  for (q = define_queue; q;)
    {
      macro_queue *oq;
      fputs ("#define ", print.outf);
      fputs (q->macro, print.outf);
      putc ('\n', print.outf);
      print.printed = false;
      print.src_line++;
      oq = q;
      q = q->next;
      free (oq->macro);
      free (oq);
    }
  define_queue = NULL;
  for (q = undef_queue; q;)
    {
      macro_queue *oq;
      fprintf (print.outf, "#undef %s\n", q->macro);
      print.src_line++;
      oq = q;
      q = q->next;
      free (oq->macro);
      free (oq);
    }
  undef_queue = NULL;
}

static void
cb_include (cpp_reader *pfile ATTRIBUTE_UNUSED, location_t line,
	    const unsigned char *dir, const char *header, int angle_brackets,
	    const cpp_token **comments)
{
  maybe_print_line (line);
  if (angle_brackets)
    fprintf (print.outf, "#%s <%s>", dir, header);
  else
    fprintf (print.outf, "#%s \"%s\"", dir, header);

  if (comments != NULL)
    {
      while (*comments != NULL)
	{
	  if ((*comments)->flags & PREV_WHITE)
	    putc (' ', print.outf);
	  cpp_output_token (*comments, print.outf);
	  ++comments;
	}
    }

  putc ('\n', print.outf);
  print.printed = false;
  print.src_line++;
}

/* Callback called when -fworking-director and -E to emit working
   directory in cpp output file.  */

void
pp_dir_change (cpp_reader *pfile ATTRIBUTE_UNUSED, const char *dir)
{
  size_t to_file_len = strlen (dir);
  unsigned char *to_file_quoted =
     (unsigned char *) alloca (to_file_len * 4 + 1);
  unsigned char *p;

  /* cpp_quote_string does not nul-terminate, so we have to do it ourselves.  */
  p = cpp_quote_string (to_file_quoted, (const unsigned char *) dir, to_file_len);
  *p = '\0';
  fprintf (print.outf, "# 1 \"%s//\"\n", to_file_quoted);
}

/* The file name, line number or system header flags have changed, as
   described in MAP.  */

void
pp_file_change (const line_map_ordinary *map)
{
  const char *flags = "";

  if (flag_no_line_commands)
    return;

  if (map != NULL)
    {
      input_location = map->start_location;
      if (print.first_time)
	{
	  /* Avoid printing foo.i when the main file is foo.c.  */
	  if (!cpp_get_options (parse_in)->preprocessed)
	    print_line (map->start_location, flags);
	  print.first_time = 0;
	}
      else
	{
	  /* Bring current file to correct line when entering a new file.  */
	  if (map->reason == LC_ENTER)
	    {
	      maybe_print_line (linemap_included_from (map));
	      flags = " 1";
	    }
	  else if (map->reason == LC_LEAVE)
	    flags = " 2";
	  print_line (map->start_location, flags);
	}
    }
}

/* Copy a #pragma directive to the preprocessed output.  */
static void
cb_def_pragma (cpp_reader *pfile, location_t line)
{
  maybe_print_line (line);
  fputs ("#pragma ", print.outf);
  cpp_output_line (pfile, print.outf);
  print.printed = false;
  print.src_line++;
}

/* Stream a token as if we had seen it directly ourselves; needed
   in case a token was lexed externally, e.g. while processing a
   pragma.  */
void
c_pp_stream_token (cpp_reader *pfile, const cpp_token *tok, location_t loc)
{
  gcc_assert (print.streamer);
  print.streamer->stream (pfile, tok, loc);
}

/* Dump out the hash table.  */
static int
dump_macro (cpp_reader *pfile, cpp_hashnode *node, void *v ATTRIBUTE_UNUSED)
{
  if (cpp_user_macro_p (node))
    {
      fputs ("#define ", print.outf);
      fputs ((const char *) cpp_macro_definition (pfile, node),
	     print.outf);
      putc ('\n', print.outf);
      print.printed = false;
      print.src_line++;
    }

  return 1;
}

/* Load in the PCH file NAME, open on FD.  It was originally searched for
   by ORIG_NAME.  Also, print out a #include command so that the PCH
   file can be loaded when the preprocessed output is compiled.  */

static void
cb_read_pch (cpp_reader *pfile, const char *name,
	     int fd, const char *orig_name ATTRIBUTE_UNUSED)
{
  c_common_read_pch (pfile, name, fd, orig_name);

  fprintf (print.outf, "#pragma GCC pch_preprocess \"%s\"\n", name);
  print.src_line++;

  /* The process of reading the PCH has destroyed the frontend parser,
     so ask the frontend to reinitialize it, in case we need it to
     process any #pragma directives encountered while preprocessing.  */
  c_init_preprocess ();
}
