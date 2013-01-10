/* Process source files and output type information.
   Copyright (C) 2006-2013 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif
#include "system.h"
#include "gengtype.h"

/* This is a simple recursive-descent parser which understands a subset of
   the C type grammar.

   Rule functions are suffixed _seq if they scan a sequence of items;
   _opt if they may consume zero tokens; _seqopt if both are true.  The
   "consume_" prefix indicates that a sequence of tokens is parsed for
   syntactic correctness and then thrown away.  */

/* Simple one-token lookahead mechanism.  */

struct token
{
  const char *value;
  int code;
  bool valid;
};
static struct token T;

/* Retrieve the code of the current token; if there is no current token,
   get the next one from the lexer.  */
static inline int
token (void)
{
  if (!T.valid)
    {
      T.code = yylex (&T.value);
      T.valid = true;
    }
  return T.code;
}

/* Retrieve the value of the current token (if any) and mark it consumed.
   The next call to token() will get another token from the lexer.  */
static inline const char *
advance (void)
{
  T.valid = false;
  return T.value;
}

/* Diagnostics.  */

/* This array is indexed by the token code minus CHAR_TOKEN_OFFSET.  */
static const char *const token_names[] = {
  "GTY",
  "typedef",
  "extern",
  "static",
  "union",
  "struct",
  "enum",
  "...",
  "ptr_alias",
  "nested_ptr",
  "a param<N>_is option",
  "a number",
  "a scalar type",
  "an identifier",
  "a string constant",
  "a character constant",
  "an array declarator",
  "a C++ keyword to ignore"
};

/* This array is indexed by token code minus FIRST_TOKEN_WITH_VALUE.  */
static const char *const token_value_format[] = {
  "%s",
  "'%s'",
  "'%s'",
  "'%s'",
  "'\"%s\"'",
  "\"'%s'\"",
  "'[%s]'",
  "'%s'",
};

/* Produce a printable representation for a token defined by CODE and
   VALUE.  This sometimes returns pointers into malloc memory and
   sometimes not, therefore it is unsafe to free the pointer it
   returns, so that memory is leaked.  This does not matter, as this
   function is only used for diagnostics, and in a successful run of
   the program there will be none.  */
static const char *
print_token (int code, const char *value)
{
  if (code < CHAR_TOKEN_OFFSET)
    return xasprintf ("'%c'", code);
  else if (code < FIRST_TOKEN_WITH_VALUE)
    return xasprintf ("'%s'", token_names[code - CHAR_TOKEN_OFFSET]);
  else if (!value)
    return token_names[code - CHAR_TOKEN_OFFSET];	/* don't quote these */
  else
    return xasprintf (token_value_format[code - FIRST_TOKEN_WITH_VALUE],
		      value);
}

/* Convenience wrapper around print_token which produces the printable
   representation of the current token.  */
static inline const char *
print_cur_token (void)
{
  return print_token (T.code, T.value);
}

/* Report a parse error on the current line, with diagnostic MSG.
   Behaves as standard printf with respect to additional arguments and
   format escapes.  */
static void ATTRIBUTE_PRINTF_1
parse_error (const char *msg, ...)
{
  va_list ap;

  fprintf (stderr, "%s:%d: parse error: ", 
	   get_input_file_name (lexer_line.file), lexer_line.line);

  va_start (ap, msg);
  vfprintf (stderr, msg, ap);
  va_end (ap);

  fputc ('\n', stderr);

  hit_error = true;
}

/* If the next token does not have code T, report a parse error; otherwise
   return the token's value.  */
static const char *
require (int t)
{
  int u = token ();
  const char *v = advance ();
  if (u != t)
    {
      parse_error ("expected %s, have %s",
		   print_token (t, 0), print_token (u, v));
      return 0;
    }
  return v;
}

/* If the next token does not have one of the codes T1 or T2, report a
   parse error; otherwise return the token's value.  */
static const char *
require2 (int t1, int t2)
{
  int u = token ();
  const char *v = advance ();
  if (u != t1 && u != t2)
    {
      parse_error ("expected %s or %s, have %s",
		   print_token (t1, 0), print_token (t2, 0),
		   print_token (u, v));
      return 0;
    }
  return v;
}

/* Near-terminals.  */

/* C-style string constant concatenation: STRING+
   Bare STRING should appear nowhere else in this file.  */
static const char *
string_seq (void)
{
  const char *s1, *s2;
  size_t l1, l2;
  char *buf;

  s1 = require (STRING);
  if (s1 == 0)
    return "";
  while (token () == STRING)
    {
      s2 = advance ();

      l1 = strlen (s1);
      l2 = strlen (s2);
      buf = XRESIZEVEC (char, CONST_CAST (char *, s1), l1 + l2 + 1);
      memcpy (buf + l1, s2, l2 + 1);
      XDELETE (CONST_CAST (char *, s2));
      s1 = buf;
    }
  return s1;
}


/* The caller has detected a template declaration that starts
   with TMPL_NAME.  Parse up to the closing '>'.  This recognizes
   simple template declarations of the form ID<ID1,ID2,...,IDn>.
   It does not try to parse anything more sophisticated than that.

   Returns the template declaration string "ID<ID1,ID2,...,IDn>".  */

static const char *
require_template_declaration (const char *tmpl_name)
{
  char *str;

  /* Recognize the opening '<'.  */
  require ('<');
  str = concat (tmpl_name, "<", (char *) 0);

  /* Read the comma-separated list of identifiers.  */
  while (token () != '>')
    {
      const char *id = require2 (ID, ',');
      if (id == NULL)
	id = ",";
      str = concat (str, id, (char *) 0);
    }

  /* Recognize the closing '>'.  */
  require ('>');
  str = concat (str, ">", (char *) 0);

  return str;
}


/* typedef_name: either an ID, or a template type
   specification of the form ID<t1,t2,...,tn>.  */

static const char *
typedef_name (void)
{
  const char *id = require (ID);
  if (token () == '<')
    return require_template_declaration (id);
  else
    return id;
}

/* Absorb a sequence of tokens delimited by balanced ()[]{}.  */
static void
consume_balanced (int opener, int closer)
{
  require (opener);
  for (;;)
    switch (token ())
      {
      default:
	advance ();
	break;
      case '(':
	consume_balanced ('(', ')');
	break;
      case '[':
	consume_balanced ('[', ']');
	break;
      case '{':
	consume_balanced ('{', '}');
	break;

      case '}':
      case ']':
      case ')':
	if (token () != closer)
	  parse_error ("unbalanced delimiters - expected '%c', have '%c'",
		       closer, token ());
      advance ();
      return;

      case EOF_TOKEN:
	parse_error ("unexpected end of file within %c%c-delimited construct",
		     opener, closer);
	return;
      }
}

/* Absorb a sequence of tokens, possibly including ()[]{}-delimited
   expressions, until we encounter an end-of-statement marker (a ';' or
   a '}') outside any such delimiters; absorb that too.  */

static void
consume_until_eos (void)
{
  for (;;)
    switch (token ())
      {
      case ';':
	advance ();
	return;

      case '{':
	consume_balanced ('{', '}');
	return;

      case '(':
	consume_balanced ('(', ')');
	break;

      case '[':
	consume_balanced ('[', ']');
	break;

      case '}':
      case ']':
      case ')':
	parse_error ("unmatched '%c' while scanning for ';'", token ());
	return;

      case EOF_TOKEN:
	parse_error ("unexpected end of file while scanning for ';'");
	return;

      default:
	advance ();
	break;
      }
}

/* Absorb a sequence of tokens, possibly including ()[]{}-delimited
   expressions, until we encounter a comma or semicolon outside any
   such delimiters; absorb that too.  Returns true if the loop ended
   with a comma.  */

static bool
consume_until_comma_or_eos ()
{
  for (;;)
    switch (token ())
      {
      case ',':
	advance ();
	return true;

      case ';':
	advance ();
	return false;

      case '{':
	consume_balanced ('{', '}');
	return false;

      case '(':
	consume_balanced ('(', ')');
	break;

      case '[':
	consume_balanced ('[', ']');
	break;

      case '}':
      case ']':
      case ')':
	parse_error ("unmatched '%s' while scanning for ',' or ';'",
		     print_cur_token ());
      return false;

      case EOF_TOKEN:
	parse_error ("unexpected end of file while scanning for ',' or ';'");
	return false;

      default:
	advance ();
	break;
      }
}


/* GTY(()) option handling.  */
static type_p type (options_p *optsp, bool nested);

/* Optional parenthesized string: ('(' string_seq ')')? */
static options_p
str_optvalue_opt (options_p prev)
{
  const char *name = advance ();
  const char *value = "";
  if (token () == '(')
    {
      advance ();
      value = string_seq ();
      require (')');
    }
  return create_string_option (prev, name, value);
}

/* absdecl: type '*'*
   -- a vague approximation to what the C standard calls an abstract
   declarator.  The only kinds that are actually used are those that
   are just a bare type and those that have trailing pointer-stars.
   Further kinds should be implemented if and when they become
   necessary.  Used only within GTY(()) option values, therefore
   further GTY(()) tags within the type are invalid.  Note that the
   return value has already been run through adjust_field_type.  */
static type_p
absdecl (void)
{
  type_p ty;
  options_p opts;

  ty = type (&opts, true);
  while (token () == '*')
    {
      ty = create_pointer (ty);
      advance ();
    }

  if (opts)
    parse_error ("nested GTY(()) options are invalid");

  return adjust_field_type (ty, 0);
}

/* Type-option: '(' absdecl ')' */
static options_p
type_optvalue (options_p prev, const char *name)
{
  type_p ty;
  require ('(');
  ty = absdecl ();
  require (')');
  return create_type_option (prev, name, ty);
}

/* Nested pointer data: '(' type '*'* ',' string_seq ',' string_seq ')' */
static options_p
nestedptr_optvalue (options_p prev)
{
  type_p ty;
  const char *from, *to;

  require ('(');
  ty = absdecl ();
  require (',');
  to = string_seq ();
  require (',');
  from = string_seq ();
  require (')');

  return create_nested_ptr_option (prev, ty, to, from);
}

/* One GTY(()) option:
   ID str_optvalue_opt
   | PTR_ALIAS type_optvalue
   | PARAM_IS type_optvalue
   | NESTED_PTR nestedptr_optvalue
*/
static options_p
option (options_p prev)
{
  switch (token ())
    {
    case ID:
      return str_optvalue_opt (prev);

    case PTR_ALIAS:
      advance ();
      return type_optvalue (prev, "ptr_alias");

    case PARAM_IS:
      return type_optvalue (prev, advance ());

    case NESTED_PTR:
      advance ();
      return nestedptr_optvalue (prev);

    case USER_GTY:
      advance ();
      return create_string_option (prev, "user", "");

    default:
      parse_error ("expected an option keyword, have %s", print_cur_token ());
      advance ();
      return create_string_option (prev, "", "");
    }
}

/* One comma-separated list of options.  */
static options_p
option_seq (void)
{
  options_p o;

  o = option (0);
  while (token () == ',')
    {
      advance ();
      o = option (o);
    }
  return o;
}

/* GTY marker: 'GTY' '(' '(' option_seq? ')' ')' */
static options_p
gtymarker (void)
{
  options_p result = 0;
  require (GTY_TOKEN);
  require ('(');
  require ('(');
  if (token () != ')')
    result = option_seq ();
  require (')');
  require (')');
  return result;
}

/* Optional GTY marker.  */
static options_p
gtymarker_opt (void)
{
  if (token () != GTY_TOKEN)
    return 0;
  return gtymarker ();
}



/* Declarators. The logic here is largely lifted from c-parser.c.
   Note that we do not have to process abstract declarators, which can
   appear only in parameter type lists or casts (but see absdecl,
   above).  Also, type qualifiers are thrown out in gengtype-lex.l so
   we don't have to do it.  */

/* array_and_function_declarators_opt:
   \epsilon
   array_and_function_declarators_opt ARRAY
   array_and_function_declarators_opt '(' ... ')'

   where '...' indicates stuff we ignore except insofar as grouping
   symbols ()[]{} must balance.

   Subroutine of direct_declarator - do not use elsewhere. */

static type_p
array_and_function_declarators_opt (type_p ty)
{
  if (token () == ARRAY)
    {
      const char *array = advance ();
      return create_array (array_and_function_declarators_opt (ty), array);
    }
  else if (token () == '(')
    {
      /* We don't need exact types for functions.  */
      consume_balanced ('(', ')');
      array_and_function_declarators_opt (ty);
      return create_scalar_type ("function type");
    }
  else
    return ty;
}

static type_p inner_declarator (type_p, const char **, options_p *, bool);

/* direct_declarator:
   '(' inner_declarator ')'
   '(' \epsilon ')'	<-- C++ ctors/dtors
   gtymarker_opt ID array_and_function_declarators_opt

   Subroutine of declarator, mutually recursive with inner_declarator;
   do not use elsewhere.

   IN_STRUCT is true if we are called while parsing structures or classes.  */

static type_p
direct_declarator (type_p ty, const char **namep, options_p *optsp,
		   bool in_struct)
{
  /* The first token in a direct-declarator must be an ID, a
     GTY marker, or an open parenthesis.  */
  switch (token ())
    {
    case GTY_TOKEN:
      *optsp = gtymarker ();
      /* fall through */

    case ID:
      *namep = require (ID);
      /* If the next token is '(', we are parsing a function declaration.
	 Functions are ignored by gengtype, so we return NULL.  */
      if (token () == '(')
	return NULL;
      break;

    case '(':
      /* If the declarator starts with a '(', we have three options.  We
	 are either parsing 'TYPE (*ID)' (i.e., a function pointer)
	 or 'TYPE(...)'.

	 The latter will be a constructor iff we are inside a
	 structure or class.  Otherwise, it could be a typedef, but
	 since we explicitly reject typedefs inside structures, we can
	 assume that we found a ctor and return NULL.  */
      advance ();
      if (in_struct && token () != '*')
	{
	  /* Found a constructor.  Find and consume the closing ')'.  */
	  while (token () != ')')
	    advance ();
	  advance ();
	  /* Tell the caller to ignore this.  */
	  return NULL;
	}
      ty = inner_declarator (ty, namep, optsp, in_struct);
      require (')');
      break;

    case IGNORABLE_CXX_KEYWORD:
      /* Any C++ keyword like 'operator' means that we are not looking
	 at a regular data declarator.  */
      return NULL;

    default:
      parse_error ("expected '(', ')', 'GTY', or an identifier, have %s",
		   print_cur_token ());
      /* Do _not_ advance if what we have is a close squiggle brace, as
	 we will get much better error recovery that way.  */
      if (token () != '}')
	advance ();
      return 0;
    }
  return array_and_function_declarators_opt (ty);
}

/* The difference between inner_declarator and declarator is in the
   handling of stars.  Consider this declaration:

   char * (*pfc) (void)

   It declares a pointer to a function that takes no arguments and
   returns a char*.  To construct the correct type for this
   declaration, the star outside the parentheses must be processed
   _before_ the function type, the star inside the parentheses must
   be processed _after_ the function type.  To accomplish this,
   declarator() creates pointers before recursing (it is actually
   coded as a while loop), whereas inner_declarator() recurses before
   creating pointers.  */

/* inner_declarator:
   '*' inner_declarator
   direct_declarator

   Mutually recursive subroutine of direct_declarator; do not use
   elsewhere.

   IN_STRUCT is true if we are called while parsing structures or classes.  */

static type_p
inner_declarator (type_p ty, const char **namep, options_p *optsp,
		  bool in_struct)
{
  if (token () == '*')
    {
      type_p inner;
      advance ();
      inner = inner_declarator (ty, namep, optsp, in_struct);
      if (inner == 0)
	return 0;
      else
	return create_pointer (ty);
    }
  else
    return direct_declarator (ty, namep, optsp, in_struct);
}

/* declarator: '*'+ direct_declarator

   This is the sole public interface to this part of the grammar.
   Arguments are the type known so far, a pointer to where the name
   may be stored, and a pointer to where GTY options may be stored.

   IN_STRUCT is true when we are called to parse declarators inside
   a structure or class.

   Returns the final type.  */

static type_p
declarator (type_p ty, const char **namep, options_p *optsp,
	    bool in_struct = false)
{
  *namep = 0;
  *optsp = 0;
  while (token () == '*')
    {
      advance ();
      ty = create_pointer (ty);
    }
  return direct_declarator (ty, namep, optsp, in_struct);
}

/* Types and declarations.  */

/* Structure field(s) declaration:
   (
   type bitfield ';'
   | type declarator bitfield? ( ',' declarator bitfield? )+ ';'
   )+

   Knows that such declarations must end with a close brace (or,
   erroneously, at EOF).
*/
static pair_p
struct_field_seq (void)
{
  pair_p f = 0;
  type_p ty, dty;
  options_p opts, dopts;
  const char *name;
  bool another;

  do
    {
      ty = type (&opts, true);

      if (!ty || token () == ':')
	{
	  consume_until_eos ();
	  continue;
	}

      do
	{
	  dty = declarator (ty, &name, &dopts, true);

	  /* There could be any number of weird things after the declarator,
	     notably bitfield declarations and __attribute__s.  If this
	     function returns true, the last thing was a comma, so we have
	     more than one declarator paired with the current type.  */
	  another = consume_until_comma_or_eos ();

	  if (!dty)
	    continue;

	  if (opts && dopts)
	    parse_error ("two GTY(()) options for field %s", name);
	  if (opts && !dopts)
	    dopts = opts;

	  f = create_field_at (f, dty, name, dopts, &lexer_line);
	}
      while (another);
    }
  while (token () != '}' && token () != EOF_TOKEN);
  return nreverse_pairs (f);
}

/* Return true if OPTS contain the option named STR.  */

static bool
opts_have (options_p opts, const char *str)
{
  for (options_p opt = opts; opt; opt = opt->next)
    if (strcmp (opt->name, str) == 0)
      return true;
  return false;
}


/* This is called type(), but what it parses (sort of) is what C calls
   declaration-specifiers and specifier-qualifier-list:

   SCALAR
   | ID     // typedef
   | (STRUCT|UNION) ID? gtymarker? ( '{' gtymarker? struct_field_seq '}' )?
   | ENUM ID ( '{' ... '}' )?

   Returns a partial type; under some conditions (notably
   "struct foo GTY((...)) thing;") it may write an options
   structure to *OPTSP.

   NESTED is true when parsing a declaration already known to have a
   GTY marker. In these cases, typedef and enum declarations are not
   allowed because gengtype only understands types at the global
   scope.  */

static type_p
type (options_p *optsp, bool nested)
{
  const char *s;
  *optsp = 0;
  switch (token ())
    {
    case SCALAR:
      s = advance ();
      return create_scalar_type (s);

    case ID:
      s = typedef_name ();
      return resolve_typedef (s, &lexer_line);

    case IGNORABLE_CXX_KEYWORD:
      /* By returning NULL here, we indicate to the caller that they
	 should ignore everything following this keyword up to the
	 next ';' or '}'.  */
      return NULL;

    case STRUCT:
    case UNION:
      {
	options_p opts = 0;
	/* GTY annotations follow attribute syntax
	   GTY_BEFORE_ID is for union/struct declarations
	   GTY_AFTER_ID is for variable declarations.  */
	enum
	{
	  NO_GTY,
	  GTY_BEFORE_ID,
	  GTY_AFTER_ID
	} is_gty = NO_GTY;
	enum typekind kind = (token () == UNION) ? TYPE_UNION : TYPE_STRUCT;
	advance ();

	/* Top-level structures that are not explicitly tagged GTY(())
	   are treated as mere forward declarations.  This is because
	   there are a lot of structures that we don't need to know
	   about, and some of those have C++ and macro constructs that
	   we cannot handle.  */
	if (nested || token () == GTY_TOKEN)
	  {
	    is_gty = GTY_BEFORE_ID;
	    opts = gtymarker_opt ();
	  }

	if (token () == ID)
	  s = advance ();
	else
	  s = xasprintf ("anonymous:%s:%d",
			 get_input_file_name (lexer_line.file),
			 lexer_line.line);

	/* Unfortunately above GTY_TOKEN check does not capture the
	   typedef struct_type GTY case.  */
	if (token () == GTY_TOKEN)
	  {
	    is_gty = GTY_AFTER_ID;
	    opts = gtymarker_opt ();
	  }

	if (token () == ':')
	  {
	    /* Skip over C++ inheritance specification.  */
	    while (token () != '{')
	      advance ();
	  }

	if (is_gty)
	  {
	    bool is_user_gty = opts_have (opts, "user");
	    if (token () == '{')
	      {
		pair_p fields;

		if (is_gty == GTY_AFTER_ID)
		  parse_error ("GTY must be specified before identifier");

		if (!is_user_gty)
		  {
		    advance ();
		    fields = struct_field_seq ();
		    require ('}');
		  }
		else
		  {
		    /* Do not look inside user defined structures.  */
		    fields = NULL;
		    kind = TYPE_USER_STRUCT;
		    consume_balanced ('{', '}');
		    return create_user_defined_type (s, &lexer_line);
		  }

		return new_structure (s, kind, &lexer_line, fields, opts);
	      }
	  }
	else if (token () == '{')
	  consume_balanced ('{', '}');
	if (opts)
	  *optsp = opts;
	return find_structure (s, kind);
      }

    case TYPEDEF:
      /* In C++, a typedef inside a struct/class/union defines a new
	 type for that inner scope.  We cannot support this in
	 gengtype because we have no concept of scoping.

	 We handle typedefs in the global scope separately (see
	 parse_file), so if we find a 'typedef', we must be inside
	 a struct.  */
      gcc_assert (nested);
      parse_error ("typedefs not supported in structures marked with "
		   "automatic GTY markers.  Use GTY((user)) to mark "
		   "this structure.");
      advance ();
      return NULL;

    case ENUM:
      advance ();
      if (token () == ID)
	s = advance ();
      else
	s = xasprintf ("anonymous:%s:%d",
		       get_input_file_name (lexer_line.file),
		       lexer_line.line);

      if (token () == '{')
	consume_balanced ('{', '}');

      /* If after parsing the enum we are at the end of the statement,
	 and we are currently inside a structure, then this was an
	 enum declaration inside this scope.

	 We cannot support this for the same reason we cannot support
	 'typedef' inside structures (see the TYPEDEF handler above).
	 If this happens, emit an error and return NULL.  */
      if (nested && token () == ';')
	{
	  parse_error ("enum definitions not supported in structures marked "
		       "with automatic GTY markers.  Use GTY((user)) to mark "
	               "this structure.");
	  advance ();
	  return NULL;
	}

      return create_scalar_type (s);

    default:
      parse_error ("expected a type specifier, have %s", print_cur_token ());
      advance ();
      return create_scalar_type ("erroneous type");
    }
}

/* Top level constructs.  */

/* Dispatch declarations beginning with 'typedef'.  */

static void
typedef_decl (void)
{
  type_p ty, dty;
  const char *name;
  options_p opts;
  bool another;

  gcc_assert (token () == TYPEDEF);
  advance ();

  ty = type (&opts, false);
  if (!ty)
    return;
  if (opts)
    parse_error ("GTY((...)) cannot be applied to a typedef");
  do
    {
      dty = declarator (ty, &name, &opts);
      if (opts)
	parse_error ("GTY((...)) cannot be applied to a typedef");

      /* Yet another place where we could have junk (notably attributes)
	 after the declarator.  */
      another = consume_until_comma_or_eos ();
      if (dty)
	do_typedef (name, dty, &lexer_line);
    }
  while (another);
}

/* Structure definition: type() does all the work.  */

static void
struct_or_union (void)
{
  options_p dummy;
  type (&dummy, false);
  /* There may be junk after the type: notably, we cannot currently
     distinguish 'struct foo *function(prototype);' from 'struct foo;'
     ...  we could call declarator(), but it's a waste of time at
     present.  Instead, just eat whatever token is currently lookahead
     and go back to lexical skipping mode. */
  advance ();
}

/* GC root declaration:
   (extern|static) gtymarker? type ID array_declarators_opt (';'|'=')
   If the gtymarker is not present, we ignore the rest of the declaration.  */
static void
extern_or_static (void)
{
  options_p opts, opts2, dopts;
  type_p ty, dty;
  const char *name;
  require2 (EXTERN, STATIC);

  if (token () != GTY_TOKEN)
    {
      advance ();
      return;
    }

  opts = gtymarker ();
  ty = type (&opts2, true);	/* if we get here, it's got a GTY(()) */
  dty = declarator (ty, &name, &dopts);

  if ((opts && dopts) || (opts && opts2) || (opts2 && dopts))
    parse_error ("GTY((...)) specified more than once for %s", name);
  else if (opts2)
    opts = opts2;
  else if (dopts)
    opts = dopts;

  if (dty)
    {
      note_variable (name, adjust_field_type (dty, opts), opts, &lexer_line);
      require2 (';', '=');
    }
}

/* Parse the file FNAME for GC-relevant declarations and definitions.
   This is the only entry point to this file.  */
void
parse_file (const char *fname)
{
  yybegin (fname);
  for (;;)
    {
      switch (token ())
	{
	case EXTERN:
	case STATIC:
	  extern_or_static ();
	  break;

	case STRUCT:
	case UNION:
	  struct_or_union ();
	  break;

	case TYPEDEF:
	  typedef_decl ();
	  break;

	case EOF_TOKEN:
	  goto eof;

	default:
	  parse_error ("unexpected top level token, %s", print_cur_token ());
	  goto eof;
	}
      lexer_toplevel_done = 1;
    }

 eof:
  advance ();
  yyend ();
}
