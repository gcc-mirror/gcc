/* Demangler for GNU C++ 
   Copyright 1989, 1991, 1994, 1995 Free Software Foundation, Inc.
   Written by James Clark (jjc@jclark.uucp)
   Rewritten by Fred Fish (fnf@cygnus.com) for ARM and Lucid demangling
   
This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This file exports two functions; cplus_mangle_opname and cplus_demangle.

   This file imports xmalloc and xrealloc, which are like malloc and
   realloc except that they generate a fatal error if there is no
   available memory. */

#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include <demangle.h>
#undef CURRENT_DEMANGLING_STYLE
#define CURRENT_DEMANGLING_STYLE work->options

extern char *xmalloc PARAMS((unsigned));
extern char *xrealloc PARAMS((char *, unsigned));

char *
mystrstr (s1, s2)
  char *s1, *s2;
{
  register char *p = s1;
  register int len = strlen (s2);

  for (; (p = strchr (p, *s2)) != 0; p++)
    {
      if (strncmp (p, s2, len) == 0)
	{
	  return (p);
	}
    }
  return (0);
}

/* In order to allow a single demangler executable to demangle strings
   using various common values of CPLUS_MARKER, as well as any specific
   one set at compile time, we maintain a string containing all the
   commonly used ones, and check to see if the marker we are looking for
   is in that string.  CPLUS_MARKER is usually '$' on systems where the
   assembler can deal with that.  Where the assembler can't, it's usually
   '.' (but on many systems '.' is used for other things).  We put the
   current defined CPLUS_MARKER first (which defaults to '$'), followed
   by the next most common value, followed by an explicit '$' in case
   the value of CPLUS_MARKER is not '$'.

   We could avoid this if we could just get g++ to tell us what the actual
   cplus marker character is as part of the debug information, perhaps by
   ensuring that it is the character that terminates the gcc<n>_compiled
   marker symbol (FIXME). */

#if !defined (CPLUS_MARKER)
#define CPLUS_MARKER '$'
#endif

enum demangling_styles current_demangling_style = gnu_demangling;

static char cplus_markers[] = { CPLUS_MARKER, '.', '$', '\0' };

void
set_cplus_marker_for_demangling (ch)
     int ch;
{
    cplus_markers[0] = ch;
}

/* Stuff that is shared between sub-routines.
 * Using a shared structure allows cplus_demangle to be reentrant. */

struct work_stuff
{
  int options;
  char **typevec;
  int ntypes;
  int typevec_size;
  int constructor;
  int destructor;
  int static_type;	/* A static member function */
  int const_type;	/* A const member function */
};

#define PRINT_ANSI_QUALIFIERS (work -> options & DMGL_ANSI)
#define PRINT_ARG_TYPES       (work -> options & DMGL_PARAMS)

static const struct optable
{
  const char *in;
  const char *out;
  int flags;
} optable[] = {
  {"nw",	  " new",	DMGL_ANSI},	/* new (1.92,	 ansi) */
  {"dl",	  " delete",	DMGL_ANSI},	/* new (1.92,	 ansi) */
  {"new",	  " new",	0},		/* old (1.91,	 and 1.x) */
  {"delete",	  " delete",	0},		/* old (1.91,	 and 1.x) */
  {"vn",	  " new []",	DMGL_ANSI},	/* GNU, pending ansi */
  {"vd",	  " delete []",	DMGL_ANSI},	/* GNU, pending ansi */
  {"as",	  "=",		DMGL_ANSI},	/* ansi */
  {"ne",	  "!=",		DMGL_ANSI},	/* old, ansi */
  {"eq",	  "==",		DMGL_ANSI},	/* old,	ansi */
  {"ge",	  ">=",		DMGL_ANSI},	/* old,	ansi */
  {"gt",	  ">",		DMGL_ANSI},	/* old,	ansi */
  {"le",	  "<=",		DMGL_ANSI},	/* old,	ansi */
  {"lt",	  "<",		DMGL_ANSI},	/* old,	ansi */
  {"plus",	  "+",		0},		/* old */
  {"pl",	  "+",		DMGL_ANSI},	/* ansi */
  {"apl",	  "+=",		DMGL_ANSI},	/* ansi */
  {"minus",	  "-",		0},		/* old */
  {"mi",	  "-",		DMGL_ANSI},	/* ansi */
  {"ami",	  "-=",		DMGL_ANSI},	/* ansi */
  {"mult",	  "*",		0},		/* old */
  {"ml",	  "*",		DMGL_ANSI},	/* ansi */
  {"amu",	  "*=",		DMGL_ANSI},	/* ansi (ARM/Lucid) */
  {"aml",	  "*=",		DMGL_ANSI},	/* ansi (GNU/g++) */
  {"convert",	  "+",		0},		/* old (unary +) */
  {"negate",	  "-",		0},		/* old (unary -) */
  {"trunc_mod",	  "%",		0},		/* old */
  {"md",	  "%",		DMGL_ANSI},	/* ansi */
  {"amd",	  "%=",		DMGL_ANSI},	/* ansi */
  {"trunc_div",	  "/",		0},		/* old */
  {"dv",	  "/",		DMGL_ANSI},	/* ansi */
  {"adv",	  "/=",		DMGL_ANSI},	/* ansi */
  {"truth_andif", "&&",		0},		/* old */
  {"aa",	  "&&",		DMGL_ANSI},	/* ansi */
  {"truth_orif",  "||",		0},		/* old */
  {"oo",	  "||",		DMGL_ANSI},	/* ansi */
  {"truth_not",	  "!",		0},		/* old */
  {"nt",	  "!",		DMGL_ANSI},	/* ansi */
  {"postincrement","++",	0},		/* old */
  {"pp",	  "++",		DMGL_ANSI},	/* ansi */
  {"postdecrement","--",	0},		/* old */
  {"mm",	  "--",		DMGL_ANSI},	/* ansi */
  {"bit_ior",	  "|",		0},		/* old */
  {"or",	  "|",		DMGL_ANSI},	/* ansi */
  {"aor",	  "|=",		DMGL_ANSI},	/* ansi */
  {"bit_xor",	  "^",		0},		/* old */
  {"er",	  "^",		DMGL_ANSI},	/* ansi */
  {"aer",	  "^=",		DMGL_ANSI},	/* ansi */
  {"bit_and",	  "&",		0},		/* old */
  {"ad",	  "&",		DMGL_ANSI},	/* ansi */
  {"aad",	  "&=",		DMGL_ANSI},	/* ansi */
  {"bit_not",	  "~",		0},		/* old */
  {"co",	  "~",		DMGL_ANSI},	/* ansi */
  {"call",	  "()",		0},		/* old */
  {"cl",	  "()",		DMGL_ANSI},	/* ansi */
  {"alshift",	  "<<",		0},		/* old */
  {"ls",	  "<<",		DMGL_ANSI},	/* ansi */
  {"als",	  "<<=",	DMGL_ANSI},	/* ansi */
  {"arshift",	  ">>",		0},		/* old */
  {"rs",	  ">>",		DMGL_ANSI},	/* ansi */
  {"ars",	  ">>=",	DMGL_ANSI},	/* ansi */
  {"component",	  "->",		0},		/* old */
  {"pt",	  "->",		DMGL_ANSI},	/* ansi; Lucid C++ form */
  {"rf",	  "->",		DMGL_ANSI},	/* ansi; ARM/GNU form */
  {"indirect",	  "*",		0},		/* old */
  {"method_call",  "->()",	0},		/* old */
  {"addr",	  "&",		0},		/* old (unary &) */
  {"array",	  "[]",		0},		/* old */
  {"vc",	  "[]",		DMGL_ANSI},	/* ansi */
  {"compound",	  ", ",		0},		/* old */
  {"cm",	  ", ",		DMGL_ANSI},	/* ansi */
  {"cond",	  "?:",		0},		/* old */
  {"cn",	  "?:",		DMGL_ANSI},	/* pseudo-ansi */
  {"max",	  ">?",		0},		/* old */
  {"mx",	  ">?",		DMGL_ANSI},	/* pseudo-ansi */
  {"min",	  "<?",		0},		/* old */
  {"mn",	  "<?",		DMGL_ANSI},	/* pseudo-ansi */
  {"nop",	  "",		0},		/* old (for operator=) */
  {"rm",	  "->*",	DMGL_ANSI}	/* ansi */
};


typedef struct string		/* Beware: these aren't required to be */
{				/*  '\0' terminated. */
  char *b;			/* pointer to start of string */
  char *p;			/* pointer after last character */
  char *e;			/* pointer after end of allocated space */
} string;

#define STRING_EMPTY(str)	((str) -> b == (str) -> p)
#define PREPEND_BLANK(str)	{if (!STRING_EMPTY(str)) \
				   string_prepend(str, " ");}
#define APPEND_BLANK(str)	{if (!STRING_EMPTY(str)) \
				   string_append(str, " ");}

#define ARM_VTABLE_STRING "__vtbl__"	/* Lucid/ARM virtual table prefix */
#define ARM_VTABLE_STRLEN 8		/* strlen (ARM_VTABLE_STRING) */

/* Prototypes for local functions */

static char *
mop_up PARAMS ((struct work_stuff *, string *, int));

#if 0
static int
demangle_method_args PARAMS ((struct work_stuff *work, const char **, string *));
#endif

static int
demangle_template PARAMS ((struct work_stuff *work, const char **, string *,
			   string *));

static int
demangle_qualified PARAMS ((struct work_stuff *, const char **, string *,
			    int, int));

static int
demangle_class PARAMS ((struct work_stuff *, const char **, string *));

static int
demangle_fund_type PARAMS ((struct work_stuff *, const char **, string *));

static int
demangle_signature PARAMS ((struct work_stuff *, const char **, string *));

static int
demangle_prefix PARAMS ((struct work_stuff *, const char **, string *));

static int
gnu_special PARAMS ((struct work_stuff *, const char **, string *));

static int
arm_special PARAMS ((struct work_stuff *, const char **, string *));

static void
string_need PARAMS ((string *, int));

static void
string_delete PARAMS ((string *));

static void
string_init PARAMS ((string *));

static void
string_clear PARAMS ((string *));

#if 0
static int
string_empty PARAMS ((string *));
#endif

static void
string_append PARAMS ((string *, const char *));

static void
string_appends PARAMS ((string *, string *));

static void
string_appendn PARAMS ((string *, const char *, int));

static void
string_prepend PARAMS ((string *, const char *));

static void
string_prependn PARAMS ((string *, const char *, int));

static int
get_count PARAMS ((const char **, int *));

static int
consume_count PARAMS ((const char **));

static int
demangle_args PARAMS ((struct work_stuff *, const char **, string *));

static int
do_type PARAMS ((struct work_stuff *, const char **, string *));

static int
do_arg PARAMS ((struct work_stuff *, const char **, string *));

static void
demangle_function_name PARAMS ((struct work_stuff *, const char **, string *,
				const char *));

static void
remember_type PARAMS ((struct work_stuff *, const char *, int));

static void
forget_types PARAMS ((struct work_stuff *));

static void
string_prepends PARAMS ((string *, string *));

/*  Translate count to integer, consuming tokens in the process.
    Conversion terminates on the first non-digit character.
    Trying to consume something that isn't a count results in
    no consumption of input and a return of 0. */

static int
consume_count (type)
    const char **type;
{
    int count = 0;

    while (isdigit (**type))
      {
	count *= 10;
	count += **type - '0';
	(*type)++;
      }
    return (count);
}

int
cplus_demangle_opname (opname, result, options)
     char *opname;
     char *result;
     int options;
{
  int len, i, len1, ret;
  string type;
  struct work_stuff work[1];
  const char *tem;

  len = strlen(opname);
  result[0] = '\0';
  ret = 0;
  work->options = options;
  
  if (opname[0] == '_' && opname[1] == '_'
	  && opname[2] == 'o' && opname[3] == 'p')
    {
      /* ANSI.  */
      /* type conversion operator.  */
      tem = opname + 4;
      if (do_type (work, &tem, &type))
	{
	  strcat (result, "operator ");
	  strncat (result, type.b, type.p - type.b);
	  string_delete (&type);
	  ret = 1;
	}
    }
  else if (opname[0] == '_' && opname[1] == '_'
	   && opname[2] >= 'a' && opname[2] <= 'z'
	   && opname[3] >= 'a' && opname[3] <= 'z')
    {
      if (opname[4] == '\0')
	{
	  /* Operator.  */
	  for (i = 0; i < sizeof (optable) / sizeof (optable[0]); i++)
	    {
	      if (strlen (optable[i].in) == 2
		  && memcmp (optable[i].in, opname + 2, 2) == 0)
		{
		  strcat (result, "operator");
		  strcat (result, optable[i].out);
		  ret = 1;
		  break;
		}
	    }
	}
      else
	{
	  if (opname[2] == 'a' && opname[5] == '\0')
	    {
	      /* Assignment. */
	      for (i = 0; i < sizeof (optable) / sizeof (optable[0]); i++)
		{
		  if (strlen (optable[i].in) == 3
		      && memcmp (optable[i].in, opname + 2, 3) == 0)
		    {
		      strcat (result, "operator");
		      strcat (result, optable[i].out);
		      ret = 1;
		      break;
		    }		      
		}
	    }
	}
    }
  else if (len >= 3 
      && opname[0] == 'o'
      && opname[1] == 'p'
      && strchr (cplus_markers, opname[2]) != NULL)
    {
      /* see if it's an assignment expression */
      if (len >= 10 /* op$assign_ */
	  && memcmp (opname + 3, "assign_", 7) == 0)
	{
	  for (i = 0; i < sizeof (optable) / sizeof (optable[0]); i++)
	    {
	      len1 = len - 10;
	      if (strlen (optable[i].in) == len1
		  && memcmp (optable[i].in, opname + 10, len1) == 0)
		{
		  strcat (result, "operator");
		  strcat (result, optable[i].out);
		  strcat (result, "=");
		  ret = 1;
		  break;
		}
	    }
	}
      else
	{
	  for (i = 0; i < sizeof (optable) / sizeof (optable[0]); i++)
	    {
	      len1 = len - 3;
	      if (strlen (optable[i].in) == len1 
		  && memcmp (optable[i].in, opname + 3, len1) == 0)
		{
		  strcat (result, "operator");
		  strcat (result, optable[i].out);
		  ret = 1;
		  break;
		}
	    }
	}
    }
  else if (len >= 5 && memcmp (opname, "type", 4) == 0
	   && strchr (cplus_markers, opname[4]) != NULL)
    {
      /* type conversion operator */
      tem = opname + 5;
      if (do_type (work, &tem, &type))
	{
	  strcat (result, "operator ");
	  strncat (result, type.b, type.p - type.b);
	  string_delete (&type);
	  ret = 1;
	}
    }
  return ret;

}
/* Takes operator name as e.g. "++" and returns mangled
   operator name (e.g. "postincrement_expr"), or NULL if not found.

   If OPTIONS & DMGL_ANSI == 1, return the ANSI name;
   if OPTIONS & DMGL_ANSI == 0, return the old GNU name.  */

char *
cplus_mangle_opname (opname, options)
     char *opname;
     int options;
{
  int i;
  int len;

  len = strlen (opname);
  for (i = 0; i < sizeof (optable) / sizeof (optable[0]); i++)
    {
      if (strlen (optable[i].out) == len
	  && (options & DMGL_ANSI) == (optable[i].flags & DMGL_ANSI)
	  && memcmp (optable[i].out, opname, len) == 0)
	return ((char *)optable[i].in);
    }
  return (0);
}

/* check to see whether MANGLED can match TEXT in the first TEXT_LEN
   characters. */

int cplus_match (mangled, text, text_len)
     const char *mangled;
     char *text;
     int text_len;
{
  if (strncmp (mangled, text, text_len) != 0) {
    return(0); /* cannot match either */
  } else {
    return(1); /* matches mangled, may match demangled */
  }
}

/* char *cplus_demangle (const char *mangled, int options)

   If MANGLED is a mangled function name produced by GNU C++, then
   a pointer to a malloced string giving a C++ representation
   of the name will be returned; otherwise NULL will be returned.
   It is the caller's responsibility to free the string which
   is returned.

   The OPTIONS arg may contain one or more of the following bits:

   	DMGL_ANSI	ANSI qualifiers such as `const' and `void' are
			included.
	DMGL_PARAMS	Function parameters are included.

   For example,
   
   cplus_demangle ("foo__1Ai", DMGL_PARAMS)		=> "A::foo(int)"
   cplus_demangle ("foo__1Ai", DMGL_PARAMS | DMGL_ANSI)	=> "A::foo(int)"
   cplus_demangle ("foo__1Ai", 0)			=> "A::foo"

   cplus_demangle ("foo__1Afe", DMGL_PARAMS)		=> "A::foo(float,...)"
   cplus_demangle ("foo__1Afe", DMGL_PARAMS | DMGL_ANSI)=> "A::foo(float,...)"
   cplus_demangle ("foo__1Afe", 0)			=> "A::foo"

   Note that any leading underscores, or other such characters prepended by
   the compilation system, are presumed to have already been stripped from
   MANGLED.  */

char *
cplus_demangle (mangled, options)
     const char *mangled;
     int options;
{
  string decl;
  int success = 0;
  struct work_stuff work[1];
  char *demangled = NULL;

  if ((mangled != NULL) && (*mangled != '\0'))
    {
      memset ((char *) work, 0, sizeof (work));
      work -> options = options;
      if ((work->options & DMGL_STYLE_MASK) == 0)
	work->options |= (int)current_demangling_style & DMGL_STYLE_MASK;
      
      string_init (&decl);

      /* First check to see if gnu style demangling is active and if the
	 string to be demangled contains a CPLUS_MARKER.  If so, attempt to
	 recognize one of the gnu special forms rather than looking for a
	 standard prefix.  In particular, don't worry about whether there
	 is a "__" string in the mangled string.  Consider "_$_5__foo" for
	 example. */

      if ((AUTO_DEMANGLING || GNU_DEMANGLING))
	{
	  success = gnu_special (work, &mangled, &decl);
	}
      if (!success)
	{
	  success = demangle_prefix (work, &mangled, &decl);
	}
      if (success && (*mangled != '\0'))
	{
	  success = demangle_signature (work, &mangled, &decl);
	}
      if (work->constructor == 2)
        {
          string_prepend(&decl, "global constructors keyed to ");
          work->constructor = 0;
        }
      else if (work->destructor == 2)
        {
          string_prepend(&decl, "global destructors keyed to ");
          work->destructor = 0;
        }
      demangled = mop_up (work, &decl, success);
    }
  return (demangled);
}

static char *
mop_up (work, declp, success)
     struct work_stuff *work;
     string *declp;
     int success;
{
  char *demangled = NULL;

  /* Discard the remembered types, if any. */
  
  forget_types (work);
  if (work -> typevec != NULL)
    {
      free ((char *) work -> typevec);
    }
  
  /* If demangling was successful, ensure that the demangled string is null
     terminated and return it.  Otherwise, free the demangling decl. */
  
  if (!success)
    {
      string_delete (declp);
    }
  else
    {
      string_appendn (declp, "", 1);
      demangled = declp -> b;
    }
  return (demangled);
}

/*

LOCAL FUNCTION

	demangle_signature -- demangle the signature part of a mangled name

SYNOPSIS

	static int
	demangle_signature (struct work_stuff *work, const char **mangled,
			    string *declp);

DESCRIPTION

	Consume and demangle the signature portion of the mangled name.

	DECLP is the string where demangled output is being built.  At
	entry it contains the demangled root name from the mangled name
	prefix.  I.E. either a demangled operator name or the root function
	name.  In some special cases, it may contain nothing.

	*MANGLED points to the current unconsumed location in the mangled
	name.  As tokens are consumed and demangling is performed, the
	pointer is updated to continuously point at the next token to
	be consumed.

	Demangling GNU style mangled names is nasty because there is no
	explicit token that marks the start of the outermost function
	argument list.
*/

static int
demangle_signature (work, mangled, declp)
     struct work_stuff *work;
     const char **mangled;
     string *declp;
{
  int success = 1;
  int func_done = 0;
  int expect_func = 0;
  const char *oldmangled = NULL;
  string trawname;
  string tname;

  while (success && (**mangled != '\0'))
    {
      switch (**mangled)
	{
	  case 'Q':
	    oldmangled = *mangled;
	    success = demangle_qualified (work, mangled, declp, 1, 0);
	    if (success)
	      {
		remember_type (work, oldmangled, *mangled - oldmangled);
	      }
	    if (AUTO_DEMANGLING || GNU_DEMANGLING)
	      {
		expect_func = 1;
	      }
	    oldmangled = NULL;
	    break;
	  
	  case 'S':
	    /* Static member function */
	    if (oldmangled == NULL)
	      {
		oldmangled = *mangled;
	      }
	    (*mangled)++;
	    work -> static_type = 1;
	    break;

	  case 'C':
	    /* a const member function */
	    if (oldmangled == NULL)
	      {
		oldmangled = *mangled;
	      }
	    (*mangled)++;
	    work -> const_type = 1;
	    break;
	  
	  case '0': case '1': case '2': case '3': case '4':
	  case '5': case '6': case '7': case '8': case '9':
	    if (oldmangled == NULL)
	      {
		oldmangled = *mangled;
	      }
	    success = demangle_class (work, mangled, declp);
	    if (success)
	      {
		remember_type (work, oldmangled, *mangled - oldmangled);
	      }
	    if (AUTO_DEMANGLING || GNU_DEMANGLING)
	      {
		expect_func = 1;
	      }
	    oldmangled = NULL;
	    break;
	  
	  case 'F':
	    /* Function */
	    /* ARM style demangling includes a specific 'F' character after
	     the class name.  For GNU style, it is just implied.  So we can
	     safely just consume any 'F' at this point and be compatible
	     with either style. */

	    oldmangled = NULL;
	    func_done = 1;
	    (*mangled)++;

	    /* For lucid/ARM style we have to forget any types we might
	       have remembered up to this point, since they were not argument
	       types.  GNU style considers all types seen as available for
	       back references.  See comment in demangle_args() */

	    if (LUCID_DEMANGLING || ARM_DEMANGLING)
	      {
		forget_types (work);
	      }
	    success = demangle_args (work, mangled, declp);
	    break;
	  
	  case 't':
	    /* G++ Template */
	    string_init(&trawname); 
	    string_init(&tname);
            if (oldmangled == NULL)
              {
                oldmangled = *mangled;
              }
	    success = demangle_template (work, mangled, &tname, &trawname);
            if (success)
              {
                remember_type (work, oldmangled, *mangled - oldmangled);
              }
	    string_append(&tname, "::");
	    string_prepends(declp, &tname);
  	    if (work -> destructor & 1)
    	      {
      		string_prepend (&trawname, "~");
      		string_appends (declp, &trawname);
		work->destructor -= 1;
    	      }
  	    if ((work->constructor & 1) || (work->destructor & 1))
    	      {
      		string_appends (declp, &trawname);
		work->constructor -= 1;
              }
	    string_delete(&trawname);
	    string_delete(&tname);
	    oldmangled = NULL;
	    expect_func = 1;
	    break;

	  case '_':
	    /* At the outermost level, we cannot have a return type specified,
	       so if we run into another '_' at this point we are dealing with
	       a mangled name that is either bogus, or has been mangled by
	       some algorithm we don't know how to deal with.  So just
	       reject the entire demangling. */
	    success = 0;
	    break;

	  default:
	    if (AUTO_DEMANGLING || GNU_DEMANGLING)
	      {
		/* Assume we have stumbled onto the first outermost function
		   argument token, and start processing args. */
		func_done = 1;
		success = demangle_args (work, mangled, declp);
	      }
	    else
	      {
		/* Non-GNU demanglers use a specific token to mark the start
		   of the outermost function argument tokens.  Typically 'F',
		   for ARM-demangling, for example.  So if we find something
		   we are not prepared for, it must be an error. */
		success = 0;
	      }
	    break;
	}
/*
      if (AUTO_DEMANGLING || GNU_DEMANGLING)
*/
	{
	  if (success && expect_func)
	    {
	      func_done = 1;
	      success = demangle_args (work, mangled, declp);
	    }
	}
    }
  if (success && !func_done)
    {
      if (AUTO_DEMANGLING || GNU_DEMANGLING)
	{
	  /* With GNU style demangling, bar__3foo is 'foo::bar(void)', and
	     bar__3fooi is 'foo::bar(int)'.  We get here when we find the
	     first case, and need to ensure that the '(void)' gets added to
	     the current declp.  Note that with ARM, the first case
	     represents the name of a static data member 'foo::bar',
	     which is in the current declp, so we leave it alone. */
	  success = demangle_args (work, mangled, declp);
	}
    }
  if (success && work -> static_type && PRINT_ARG_TYPES)
    {
      string_append (declp, " static");
    }
  if (success && work -> const_type && PRINT_ARG_TYPES)
    {
      string_append (declp, " const");
    }
  return (success);
}

#if 0

static int
demangle_method_args (work, mangled, declp)
     struct work_stuff *work;
     const char **mangled;
     string *declp;
{
  int success = 0;

  if (work -> static_type)
    {
      string_append (declp, *mangled + 1);
      *mangled += strlen (*mangled);
      success = 1;
    }
  else
    {
      success = demangle_args (work, mangled, declp);
    }
  return (success);
}

#endif

static int
demangle_template (work, mangled, tname, trawname)
     struct work_stuff *work;
     const char **mangled;
     string *tname;
     string *trawname;
{
  int i;
  int is_pointer;
  int is_real;
  int is_integral;
  int is_char;
  int is_bool;
  int r;
  int need_comma = 0;
  int success = 0;
  int done;
  const char *old_p;
  const char *start;
  int symbol_len;
  string temp;

  (*mangled)++;
  start = *mangled;
  /* get template name */
  if ((r = consume_count (mangled)) == 0 || strlen (*mangled) < r)
    {
      return (0);
    }
  if (trawname)
    string_appendn (trawname, *mangled, r);
  string_appendn (tname, *mangled, r);
  *mangled += r;
  string_append (tname, "<");
  /* get size of template parameter list */
  if (!get_count (mangled, &r))
    {
      return (0);
    }
  for (i = 0; i < r; i++)
    {
      if (need_comma)
	{
	  string_append (tname, ", ");
	}
      /* Z for type parameters */
      if (**mangled == 'Z')
	{
	  (*mangled)++;
	  /* temp is initialized in do_type */
	  success = do_type (work, mangled, &temp);
	  if (success)
	    {
	      string_appends (tname, &temp);
	    }
	  string_delete(&temp);
	  if (!success)
	    {
	      break;
	    }
	}
      else
	{
	  /* otherwise, value parameter */
	  old_p  = *mangled;
	  is_pointer = 0;
	  is_real = 0;
	  is_integral = 0;
          is_char = 0;
	  done = 0;
	  /* temp is initialized in do_type */
	  success = do_type (work, mangled, &temp);
/*
	  if (success)
	    {
	      string_appends (tname, &temp);
	    }
*/
	  string_delete(&temp);
	  if (!success)
	    {
	      break;
	    }
/*
	  string_append (tname, "=");
*/
	  while (*old_p && !done)
	    {	
	      switch (*old_p)
		{
		  case 'P':
		  case 'p':
		  case 'R':
		    done = is_pointer = 1;
		    break;
		  case 'C':	/* const */
		  case 'S':	/* explicitly signed [char] */
		  case 'U':	/* unsigned */
		  case 'V':	/* volatile */
		  case 'F':	/* function */
		  case 'M':	/* member function */
		  case 'O':	/* ??? */
		    old_p++;
		    continue;
		  case 'Q':	/* qualified name */
                    done = is_integral = 1;
                    break;
		  case 'T':	/* remembered type */
		    abort ();
		    break;
		  case 'v':	/* void */
		    abort ();
		    break;
		  case 'x':	/* long long */
		  case 'l':	/* long */
		  case 'i':	/* int */
		  case 's':	/* short */
		  case 'w':	/* wchar_t */
		    done = is_integral = 1;
		    break;
		  case 'b':	/* bool */
		    done = is_bool = 1;
		    break;
		  case 'c':	/* char */
		    done = is_char = 1;
		    break;
		  case 'r':	/* long double */
		  case 'd':	/* double */
		  case 'f':	/* float */
		    done = is_real = 1;
		    break;
		  default:
		    /* it's probably user defined type, let's assume
		       it's integral, it seems hard to figure out
		       what it really is */
		    done = is_integral = 1;
		}
	    }
	  if (is_integral)
	    {
	      if (**mangled == 'm')
		{
		  string_appendn (tname, "-", 1);
		  (*mangled)++;
		}
	      while (isdigit (**mangled))	
		{
		  string_appendn (tname, *mangled, 1);
		  (*mangled)++;
		}
	    }
	  else if (is_char)
	    {
            char tmp[2];
            int val;
              if (**mangled == 'm')
                {
                  string_appendn (tname, "-", 1);
                  (*mangled)++;
                }
	      string_appendn (tname, "'", 1);
              val = consume_count(mangled);
	      if (val == 0)
		{
		  success = 0;
		  break;
                }
              tmp[0] = (char)val;
              tmp[1] = '\0';
              string_appendn (tname, &tmp[0], 1);
	      string_appendn (tname, "'", 1);
	    }
	  else if (is_bool)
	    {
	      int val = consume_count (mangled);
	      if (val == 0)
		string_appendn (tname, "false", 5);
	      else if (val == 1)
		string_appendn (tname, "true", 4);
	      else
		success = 0;
	    }
	  else if (is_real)
	    {
	      if (**mangled == 'm')
		{
		  string_appendn (tname, "-", 1);
		  (*mangled)++;
		}
	      while (isdigit (**mangled))	
		{
		  string_appendn (tname, *mangled, 1);
		  (*mangled)++;
		}
	      if (**mangled == '.') /* fraction */
		{
		  string_appendn (tname, ".", 1);
		  (*mangled)++;
		  while (isdigit (**mangled))	
		    {
		      string_appendn (tname, *mangled, 1);
		      (*mangled)++;
		    }
		}
	      if (**mangled == 'e') /* exponent */
		{
		  string_appendn (tname, "e", 1);
		  (*mangled)++;
		  while (isdigit (**mangled))	
		    {
		      string_appendn (tname, *mangled, 1);
		      (*mangled)++;
		    }
		}
	    }
	  else if (is_pointer)
	    {
	      if (!get_count (mangled, &symbol_len))
		{
		  success = 0;
		  break;
		}
	      string_appendn (tname, *mangled, symbol_len);
	      *mangled += symbol_len;
	    }
	}
      need_comma = 1;
    }
  if (tname->p[-1] == '>')
    string_append (tname, " ");
  string_append (tname, ">");
  
/*
      if (work -> static_type)
	{
	  string_append (declp, *mangled + 1);
	  *mangled += strlen (*mangled);
	  success = 1;
	}
      else
	{
	  success = demangle_args (work, mangled, declp);
	}
    }
*/
  return (success);
}

static int
arm_pt (work, mangled, n, anchor, args)
     struct work_stuff *work;
     const char *mangled;
     int n;
     const char **anchor, **args;
{
  /* ARM template? */
  if (ARM_DEMANGLING && (*anchor = mystrstr (mangled, "__pt__")))
    {
	int len;
        *args = *anchor + 6;
	len = consume_count (args);
        if (*args + len == mangled + n && **args == '_')
	  {
	    ++*args;
	    return 1;
	  }
    }
  return 0;
}

static void
demangle_arm_pt (work, mangled, n, declp)
     struct work_stuff *work;
     const char **mangled;
     int n;
     string *declp;
{
  const char *p;
  const char *args;
  const char *e = *mangled + n;

  /* ARM template? */
  if (arm_pt (work, *mangled, n, &p, &args))
  {
    string arg;
    string_init (&arg);
    string_appendn (declp, *mangled, p - *mangled);
    string_append (declp, "<");
    /* should do error checking here */
    while (args < e) {
      string_clear (&arg);
      do_type (work, &args, &arg);
      string_appends (declp, &arg);
      string_append (declp, ",");
    }
    string_delete (&arg);
    --declp->p;
    string_append (declp, ">");
  }
  else
  {
    string_appendn (declp, *mangled, n);
  }
  *mangled += n;
}

static int
demangle_class_name (work, mangled, declp)
     struct work_stuff *work;
     const char **mangled;
     string *declp;
{
  int n;
  int success = 0;

  n = consume_count (mangled);
  if (strlen (*mangled) >= n)
  {
    demangle_arm_pt (work, mangled, n, declp);
    success = 1;
  }

  return (success);
}

/*

LOCAL FUNCTION

	demangle_class -- demangle a mangled class sequence

SYNOPSIS

	static int
	demangle_class (struct work_stuff *work, const char **mangled,
			strint *declp)

DESCRIPTION

	DECLP points to the buffer into which demangling is being done.

	*MANGLED points to the current token to be demangled.  On input,
	it points to a mangled class (I.E. "3foo", "13verylongclass", etc.)
	On exit, it points to the next token after the mangled class on
	success, or the first unconsumed token on failure.

	If the CONSTRUCTOR or DESTRUCTOR flags are set in WORK, then
	we are demangling a constructor or destructor.  In this case
	we prepend "class::class" or "class::~class" to DECLP.

	Otherwise, we prepend "class::" to the current DECLP.

	Reset the constructor/destructor flags once they have been
	"consumed".  This allows demangle_class to be called later during
	the same demangling, to do normal class demangling.

	Returns 1 if demangling is successful, 0 otherwise.

*/

static int
demangle_class (work, mangled, declp)
     struct work_stuff *work;
     const char **mangled;
     string *declp;
{
  int success = 0;
  string class_name;

  string_init (&class_name);
  if (demangle_class_name (work, mangled, &class_name))
    {
      if ((work->constructor & 1) || (work->destructor & 1))
	{
	  string_prepends (declp, &class_name);
	  if (work -> destructor & 1)
	    {
	      string_prepend (declp, "~");
              work -> destructor -= 1;
	    }
	  else
	    {
	      work -> constructor -= 1; 
	    }
	}
      string_prepend (declp, "::");
      string_prepends (declp, &class_name);
      success = 1;
    }
  string_delete (&class_name);
  return (success);
}

/*

LOCAL FUNCTION

	demangle_prefix -- consume the mangled name prefix and find signature

SYNOPSIS

	static int
	demangle_prefix (struct work_stuff *work, const char **mangled,
			 string *declp);

DESCRIPTION

	Consume and demangle the prefix of the mangled name.

	DECLP points to the string buffer into which demangled output is
	placed.  On entry, the buffer is empty.  On exit it contains
	the root function name, the demangled operator name, or in some
	special cases either nothing or the completely demangled result.

	MANGLED points to the current pointer into the mangled name.  As each
	token of the mangled name is consumed, it is updated.  Upon entry
	the current mangled name pointer points to the first character of
	the mangled name.  Upon exit, it should point to the first character
	of the signature if demangling was successful, or to the first
	unconsumed character if demangling of the prefix was unsuccessful.
	
	Returns 1 on success, 0 otherwise.
 */

static int
demangle_prefix (work, mangled, declp)
     struct work_stuff *work;
     const char **mangled;
     string *declp;
{
  int success = 1;
  const char *scan;
  int i;

  if (strlen(*mangled) >= 11 && strncmp(*mangled, "_GLOBAL_", 8) == 0)
    {
      char *marker = strchr (cplus_markers, (*mangled)[8]);
      if (marker != NULL && *marker == (*mangled)[10])
	{
	  if ((*mangled)[9] == 'D')
	    {
	      /* it's a GNU global destructor to be executed at program exit */
	      (*mangled) += 11;
	      work->destructor = 2;
	      if (gnu_special (work, mangled, declp))
		return success;
	    }
	  else if ((*mangled)[9] == 'I')
	    {
	      /* it's a GNU global constructor to be executed at program init */
	      (*mangled) += 11;
	      work->constructor = 2;
	      if (gnu_special (work, mangled, declp))
		return success;
	    }
	}
    }
  else if (ARM_DEMANGLING && strncmp(*mangled, "__std__", 7) == 0)
    {
      /* it's a ARM global destructor to be executed at program exit */
      (*mangled) += 7;
      work->destructor = 2;
    }
  else if (ARM_DEMANGLING && strncmp(*mangled, "__sti__", 7) == 0)
    {
      /* it's a ARM global constructor to be executed at program initial */
      (*mangled) += 7;
      work->constructor = 2;
    }

/*  This block of code is a reduction in strength time optimization
    of:
    	scan = mystrstr (*mangled, "__"); */

  {
    scan = *mangled;

    do {
      scan = strchr (scan, '_');
    } while (scan != NULL && *++scan != '_');

    if (scan != NULL) --scan;
  }

  if (scan != NULL)
    {
      /* We found a sequence of two or more '_', ensure that we start at
	 the last pair in the sequence. */
      i = strspn (scan, "_");
      if (i > 2)
	{
	  scan += (i - 2); 
	}
    }
 
  if (scan == NULL)
    {
      success = 0;
    }
  else if (work -> static_type)
    {
      if (!isdigit (scan[0]) && (scan[0] != 't'))
	{
	  success = 0;
	}
    }
  else if ((scan == *mangled) &&
	   (isdigit (scan[2]) || (scan[2] == 'Q') || (scan[2] == 't')))
    {
      /* The ARM says nothing about the mangling of local variables.
	 But cfront mangles local variables by prepending __<nesting_level>
	 to them. As an extension to ARM demangling we handle this case.  */
      if ((LUCID_DEMANGLING || ARM_DEMANGLING) && isdigit (scan[2]))
	{
	  *mangled = scan + 2;
	  consume_count (mangled);
	  string_append (declp, *mangled);
	  *mangled += strlen (*mangled);
	  success = 1; 
	}
      else
	{
	  /* A GNU style constructor starts with __[0-9Qt].  But cfront uses
	     names like __Q2_3foo3bar for nested type names.  So don't accept
	     this style of constructor for cfront demangling.  */
	  if (!(LUCID_DEMANGLING || ARM_DEMANGLING))
	    work -> constructor += 1;
	  *mangled = scan + 2;
	}
    }
  else if ((scan == *mangled) && !isdigit (scan[2]) && (scan[2] != 't'))
    {
      /* Mangled name starts with "__".  Skip over any leading '_' characters,
	 then find the next "__" that separates the prefix from the signature.
	 */
      if (!(ARM_DEMANGLING || LUCID_DEMANGLING)
	  || (arm_special (work, mangled, declp) == 0))
	{
	  while (*scan == '_')
	    {
	      scan++;
	    }
	  if ((scan = mystrstr (scan, "__")) == NULL || (*(scan + 2) == '\0'))
	    {
	      /* No separator (I.E. "__not_mangled"), or empty signature
		 (I.E. "__not_mangled_either__") */
	      success = 0;
	    }
	  else
	    {
	      demangle_function_name (work, mangled, declp, scan);
	    }
	}
    }
  else if (ARM_DEMANGLING && scan[2] == 'p' && scan[3] == 't')
    {
      /* Cfront-style parameterized type.  Handled later as a signature. */
      success = 1;

      /* ARM template? */
      demangle_arm_pt (work, mangled, strlen (*mangled), declp);
    }
  else if (*(scan + 2) != '\0')
    {
      /* Mangled name does not start with "__" but does have one somewhere
	 in there with non empty stuff after it.  Looks like a global
	 function name. */
      demangle_function_name (work, mangled, declp, scan);
    }
  else
    {
      /* Doesn't look like a mangled name */
      success = 0;
    }

  if (!success && (work->constructor == 2 || work->destructor == 2))
    {
      string_append (declp, *mangled);
      *mangled += strlen (*mangled);
      success = 1;
    } 
  return (success);
}

/*

LOCAL FUNCTION

	gnu_special -- special handling of gnu mangled strings

SYNOPSIS

	static int
	gnu_special (struct work_stuff *work, const char **mangled,
		     string *declp);


DESCRIPTION

	Process some special GNU style mangling forms that don't fit
	the normal pattern.  For example:

		_$_3foo		(destructor for class foo)
		_vt$foo		(foo virtual table)
		_vt$foo$bar	(foo::bar virtual table)
		__vt_foo	(foo virtual table, new style with thunks)
		_3foo$varname	(static data member)
		_Q22rs2tu$vw	(static data member)
		__t6vector1Zii	(constructor with template)
		__thunk_4__$_7ostream (virtual function thunk)
 */

static int
gnu_special (work, mangled, declp)
     struct work_stuff *work;
     const char **mangled;
     string *declp;
{
  int n;
  int success = 1;
  const char *p;

  if ((*mangled)[0] == '_'
      && strchr (cplus_markers, (*mangled)[1]) != NULL
      && (*mangled)[2] == '_')
    {
      /* Found a GNU style destructor, get past "_<CPLUS_MARKER>_" */
      (*mangled) += 3;
      work -> destructor += 1;
    }
  else if ((*mangled)[0] == '_'
	   && (((*mangled)[1] == '_'
		&& (*mangled)[2] == 'v'
		&& (*mangled)[3] == 't'
		&& (*mangled)[4] == '_')
	     || ((*mangled)[1] == 'v'
		 && (*mangled)[2] == 't'
		 && strchr (cplus_markers, (*mangled)[3]) != NULL)))
    {
      /* Found a GNU style virtual table, get past "_vt<CPLUS_MARKER>"
         and create the decl.  Note that we consume the entire mangled
	 input string, which means that demangle_signature has no work
	 to do. */
      if ((*mangled)[2] == 'v')
	(*mangled) += 5; /* New style, with thunks: "__vt_" */
      else
	(*mangled) += 4; /* Old style, no thunks: "_vt<CPLUS_MARKER>" */
      while (**mangled != '\0')
	{
	  p = strpbrk (*mangled, cplus_markers);
	  switch (**mangled)
	    {
	    case 'Q':
	      success = demangle_qualified (work, mangled, declp, 0, 1);
	      break;
	    case 't':
	      success = demangle_template (work, mangled, declp, 0);
	      break;
	    default:
	      if (isdigit(*mangled[0]))
		{
		  n = consume_count(mangled);
		}
	      else
		{
		  n = strcspn (*mangled, cplus_markers);
		}
	      string_appendn (declp, *mangled, n);
	      (*mangled) += n;
	    }

	  if (success && ((p == NULL) || (p == *mangled)))
	    {
	      if (p != NULL)
		{
		  string_append (declp, "::");
		  (*mangled)++;
		}
	    }
	  else
	    {
	      success = 0;
	      break;
	    }
	}
      if (success)
	string_append (declp, " virtual table");
    }
  else if ((*mangled)[0] == '_'
	   && (strchr("0123456789Qt", (*mangled)[1]) != NULL)
	   && (p = strpbrk (*mangled, cplus_markers)) != NULL)
    {
      /* static data member, "_3foo$varname" for example */
      (*mangled)++;
      switch (**mangled)
	{
	  case 'Q':
	    success = demangle_qualified (work, mangled, declp, 0, 1);
	    break;
	  case 't':
	    success = demangle_template (work, mangled, declp, 0);
	    break;
	  default:
	    n = consume_count (mangled);
	    string_appendn (declp, *mangled, n);
	    (*mangled) += n;
	}
      if (success && (p == *mangled))
	{
	  /* Consumed everything up to the cplus_marker, append the
	     variable name. */
	  (*mangled)++;
	  string_append (declp, "::");
	  n = strlen (*mangled);
	  string_appendn (declp, *mangled, n);
	  (*mangled) += n;
	}
      else
	{
	  success = 0;
	}
    }
  else if (strncmp (*mangled, "__thunk_", 8) == 0)
    {
      int delta = ((*mangled) += 8, consume_count (mangled));
      char *method = cplus_demangle (++*mangled, work->options);
      if (method)
	{
	  char buf[50];
	  sprintf (buf, "virtual function thunk (delta:%d) for ", -delta);
	  string_append (declp, buf);
	  string_append (declp, method);
	  free (method);
	  n = strlen (*mangled);
	  (*mangled) += n;
	}
      else
	{
	  success = 0;
	}
    }
  else
    {
      success = 0;
    }
  return (success);
}

/*

LOCAL FUNCTION

	arm_special -- special handling of ARM/lucid mangled strings

SYNOPSIS

	static int
	arm_special (struct work_stuff *work, const char **mangled,
			string *declp);


DESCRIPTION

	Process some special ARM style mangling forms that don't fit
	the normal pattern.  For example:

		__vtbl__3foo		(foo virtual table)
		__vtbl__3foo__3bar	(bar::foo virtual table)

 */

static int
arm_special (work, mangled, declp)
     struct work_stuff *work;
     const char **mangled;
     string *declp;
{
  int n;
  int success = 1;
  const char *scan;

  if (strncmp (*mangled, ARM_VTABLE_STRING, ARM_VTABLE_STRLEN) == 0)
    {
      /* Found a ARM style virtual table, get past ARM_VTABLE_STRING
         and create the decl.  Note that we consume the entire mangled
	 input string, which means that demangle_signature has no work
	 to do. */
      scan = *mangled + ARM_VTABLE_STRLEN;
      while (*scan != '\0')        /* first check it can be demangled */
        {
          n = consume_count (&scan);
          if (n==0)
	    {
	      return (0);           /* no good */
	    }
          scan += n;
          if (scan[0] == '_' && scan[1] == '_')
	    {
	      scan += 2;
	    }
        }
      (*mangled) += ARM_VTABLE_STRLEN;
      while (**mangled != '\0')
	{
	  n = consume_count (mangled);
	  string_prependn (declp, *mangled, n);
	  (*mangled) += n;
	  if ((*mangled)[0] == '_' && (*mangled)[1] == '_')
	    {
	      string_prepend (declp, "::");
	      (*mangled) += 2;
	    }
	}
      string_append (declp, " virtual table");
    }
  else
    {
      success = 0;
    }
  return (success);
}

/*

LOCAL FUNCTION

	demangle_qualified -- demangle 'Q' qualified name strings

SYNOPSIS

	static int
	demangle_qualified (struct work_stuff *, const char *mangled,
			    string *result, int isfuncname, int append);

DESCRIPTION

	Demangle a qualified name, such as "Q25Outer5Inner" which is
	the mangled form of "Outer::Inner".  The demangled output is
	prepended or appended to the result string according to the
	state of the append flag.

	If isfuncname is nonzero, then the qualified name we are building
	is going to be used as a member function name, so if it is a
	constructor or destructor function, append an appropriate
	constructor or destructor name.  I.E. for the above example,
	the result for use as a constructor is "Outer::Inner::Inner"
	and the result for use as a destructor is "Outer::Inner::~Inner".

BUGS

	Numeric conversion is ASCII dependent (FIXME).

 */

static int
demangle_qualified (work, mangled, result, isfuncname, append)
     struct work_stuff *work;
     const char **mangled;
     string *result;
     int isfuncname;
     int append;
{
  int qualifiers;
  int namelength;
  int success = 1;
  const char *p;
  char num[2];
  string temp;

  string_init (&temp);
  switch ((*mangled)[1])
    {
    case '_':
      /* GNU mangled name with more than 9 classes.  The count is preceded
	 by an underscore (to distinguish it from the <= 9 case) and followed
	 by an underscore.  */
      p = *mangled + 2;
      qualifiers = atoi (p);
      if (!isdigit (*p) || *p == '0')
	success = 0;

      /* Skip the digits.  */
      while (isdigit (*p))
	++p;

      if (*p != '_')
	success = 0;

      *mangled = p + 1;
      break;

    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      /* The count is in a single digit.  */
      num[0] = (*mangled)[1];
      num[1] = '\0';
      qualifiers = atoi (num);

      /* If there is an underscore after the digit, skip it.  This is
	 said to be for ARM-qualified names, but the ARM makes no
	 mention of such an underscore.  Perhaps cfront uses one.  */
      if ((*mangled)[2] == '_')
	{
	  (*mangled)++;
	}
      (*mangled) += 2;
      break;

    case '0':
    default:
      success = 0;
    }

  if (!success)
    return success;

  /* Pick off the names and collect them in the temp buffer in the order
     in which they are found, separated by '::'. */

  while (qualifiers-- > 0)
    {
      if (*mangled[0] == '_') 
	*mangled = *mangled + 1;
      if (*mangled[0] == 't')
	{
	  success = demangle_template(work, mangled, &temp, 0);
	  if (!success) break;
	}
      else
        {	
	  namelength = consume_count (mangled);
      	  if (strlen (*mangled) < namelength)
	    {
	    /* Simple sanity check failed */
	       success = 0;
	       break;
	    }
      	  string_appendn (&temp, *mangled, namelength);
      	  *mangled += namelength;
	}
      if (qualifiers > 0)
        {
          string_appendn (&temp, "::", 2);
        }
    }

  /* If we are using the result as a function name, we need to append
     the appropriate '::' separated constructor or destructor name.
     We do this here because this is the most convenient place, where
     we already have a pointer to the name and the length of the name. */

  if (isfuncname && (work->constructor & 1 || work->destructor & 1))
    {
      string_appendn (&temp, "::", 2);
      if (work -> destructor & 1)
	{
	  string_append (&temp, "~");
	}
      string_appendn (&temp, (*mangled) - namelength, namelength);
    }

  /* Now either prepend the temp buffer to the result, or append it, 
     depending upon the state of the append flag. */

  if (append)
    {
      string_appends (result, &temp);
    }
  else
    {
      if (!STRING_EMPTY (result))
	{
	  string_appendn (&temp, "::", 2);
	}
      string_prepends (result, &temp);
    }

  string_delete (&temp);
  return (success);
}

/*

LOCAL FUNCTION

	get_count -- convert an ascii count to integer, consuming tokens

SYNOPSIS

	static int
	get_count (const char **type, int *count)

DESCRIPTION

	Return 0 if no conversion is performed, 1 if a string is converted.
*/

static int
get_count (type, count)
     const char **type;
     int *count;
{
  const char *p;
  int n;

  if (!isdigit (**type))
    {
      return (0);
    }
  else
    {
      *count = **type - '0';
      (*type)++;
      if (isdigit (**type))
	{
	  p = *type;
	  n = *count;
	  do 
	    {
	      n *= 10;
	      n += *p - '0';
	      p++;
	    } 
	  while (isdigit (*p));
	  if (*p == '_')
	    {
	      *type = p + 1;
	      *count = n;
	    }
	}
    }
  return (1);
}

/* result will be initialised here; it will be freed on failure */

static int
do_type (work, mangled, result)
     struct work_stuff *work;
     const char **mangled;
     string *result;
{
  int n;
  int done;
  int success;
  string decl;
  const char *remembered_type;
  int constp;
  int volatilep;

  string_init (&decl);
  string_init (result);

  done = 0;
  success = 1;
  while (success && !done)
    {
      int member;
      switch (**mangled)
	{

	/* A pointer type */
	case 'P':
	case 'p':
	  (*mangled)++;
	  string_prepend (&decl, "*");
	  break;

	/* A reference type */
	case 'R':
	  (*mangled)++;
	  string_prepend (&decl, "&");
	  break;

	/* An array */
	case 'A':
	  {
	    const char *p = ++(*mangled);

	    string_prepend (&decl, "(");
	    string_append (&decl, ")[");
	    /* Copy anything up until the next underscore (the size of the
	       array).  */
	    while (**mangled && **mangled != '_')
	      ++(*mangled);
	    if (**mangled == '_')
	      {
		string_appendn (&decl, p, *mangled - p);
		string_append (&decl, "]");             
		*mangled += 1;
	      }
	    else
	      success = 0;
	    break;
	  }

	/* A back reference to a previously seen type */
	case 'T':
	  (*mangled)++;
	  if (!get_count (mangled, &n) || n >= work -> ntypes)
	    {
	      success = 0;
	    }
	  else
	    {
	      remembered_type = work -> typevec[n];
	      mangled = &remembered_type;
	    }
	  break;

	/* A function */
	case 'F':
	  (*mangled)++;
	  if (!STRING_EMPTY (&decl) && decl.b[0] == '*')
	    {
	      string_prepend (&decl, "(");
	      string_append (&decl, ")");
	    }
	  /* After picking off the function args, we expect to either find the
	     function return type (preceded by an '_') or the end of the
	     string. */
	  if (!demangle_args (work, mangled, &decl)
	      || (**mangled != '_' && **mangled != '\0'))
	    {
	      success = 0;
	    }
	  if (success && (**mangled == '_'))
	    {
	      (*mangled)++;
	    }
	  break;

	case 'M':
	case 'O':
	  {
	    constp = 0;
	    volatilep = 0;

	    member = **mangled == 'M';
	    (*mangled)++;
	    if (!isdigit (**mangled))
	      {
		success = 0;
		break;
	      }
	    n = consume_count (mangled);
	    if (strlen (*mangled) < n)
	      {
		success = 0;
		break;
	      }
	    string_append (&decl, ")");
	    string_prepend (&decl, "::");
	    string_prependn (&decl, *mangled, n);
	    string_prepend (&decl, "(");
	    *mangled += n;
	    if (member)
	      {
		if (**mangled == 'C')
		  {
		    (*mangled)++;
		    constp = 1;
		  }
		if (**mangled == 'V')
		  {
		    (*mangled)++;
		    volatilep = 1;
		  }
		if (*(*mangled)++ != 'F')
		  {
		    success = 0;
		    break;
		  }
	      }
	    if ((member && !demangle_args (work, mangled, &decl))
		|| **mangled != '_')
	      {
		success = 0;
		break;
	      }
	    (*mangled)++;
	    if (! PRINT_ANSI_QUALIFIERS)
	      {
		break;
	      }
	    if (constp)
	      {
		APPEND_BLANK (&decl);
		string_append (&decl, "const");
	      }
	    if (volatilep)
	      {
		APPEND_BLANK (&decl);
		string_append (&decl, "volatile");
	      }
	    break;
	  }
        case 'G':
	    (*mangled)++;
	    break;

	case 'C':
	  (*mangled)++;
/*
	  if ((*mangled)[1] == 'P')
	    {
*/
	      if (PRINT_ANSI_QUALIFIERS)
		{
		  if (!STRING_EMPTY (&decl))
		    {
		      string_prepend (&decl, " ");
		    }
		  string_prepend (&decl, "const");
		}
	      break;
/*
	    }
*/

	  /* fall through */
	default:
	  done = 1;
	  break;
	}
    }

  switch (**mangled)
    {
      /* A qualified name, such as "Outer::Inner". */
      case 'Q':
        success = demangle_qualified (work, mangled, result, 0, 1);
	break;

      default:
	success = demangle_fund_type (work, mangled, result);
	break;
    }

  if (success)
    {
      if (!STRING_EMPTY (&decl))
	{
	  string_append (result, " ");
	  string_appends (result, &decl);
	}
    }
  else
    {
      string_delete (result);
    }
  string_delete (&decl);
  return (success);
}

/* Given a pointer to a type string that represents a fundamental type
   argument (int, long, unsigned int, etc) in TYPE, a pointer to the
   string in which the demangled output is being built in RESULT, and
   the WORK structure, decode the types and add them to the result.

   For example:

   	"Ci"	=>	"const int"
	"Sl"	=>	"signed long"
	"CUs"	=>	"const unsigned short"

   */

static int
demangle_fund_type (work, mangled, result)
     struct work_stuff *work;
     const char **mangled;
     string *result;
{
  int done = 0;
  int success = 1;

  /* First pick off any type qualifiers.  There can be more than one. */

  while (!done)
    {
      switch (**mangled)
	{
	  case 'C':
	    (*mangled)++;
	    if (PRINT_ANSI_QUALIFIERS)
	      {
		APPEND_BLANK (result);
		string_append (result, "const");
	      }
	    break;
	  case 'U':
	    (*mangled)++;
	    APPEND_BLANK (result);
	    string_append (result, "unsigned");
	    break;
	  case 'S': /* signed char only */
	    (*mangled)++;
	    APPEND_BLANK (result);
	    string_append (result, "signed");
	    break;
	  case 'V':
	    (*mangled)++;
	    if (PRINT_ANSI_QUALIFIERS)
	      {
		APPEND_BLANK (result);
		string_append (result, "volatile");
	      }
	    break;
	  default:
	    done = 1;
	    break;
	}
    }

  /* Now pick off the fundamental type.  There can be only one. */

  switch (**mangled)
    {
      case '\0':
      case '_':
	break;
      case 'v':
	(*mangled)++;
	APPEND_BLANK (result);
	string_append (result, "void");
	break;
      case 'x':
	(*mangled)++;
	APPEND_BLANK (result);
	string_append (result, "long long");
	break;
      case 'l':
	(*mangled)++;
	APPEND_BLANK (result);
	string_append (result, "long");
	break;
      case 'i':
	(*mangled)++;
	APPEND_BLANK (result);
	string_append (result, "int");
	break;
      case 's':
	(*mangled)++;
	APPEND_BLANK (result);
	string_append (result, "short");
	break;
      case 'b':
	(*mangled)++;
	APPEND_BLANK (result);
	string_append (result, "bool");
	break;
      case 'c':
	(*mangled)++;
	APPEND_BLANK (result);
	string_append (result, "char");
	break;
      case 'w':
	(*mangled)++;
	APPEND_BLANK (result);
	string_append (result, "wchar_t");
	break;
      case 'r':
	(*mangled)++;
	APPEND_BLANK (result);
	string_append (result, "long double");
	break;
      case 'd':
	(*mangled)++;
	APPEND_BLANK (result);
	string_append (result, "double");
	break;
      case 'f':
	(*mangled)++;
	APPEND_BLANK (result);
	string_append (result, "float");
	break;
      case 'G':
	(*mangled)++;
	if (!isdigit (**mangled))
	  {
	    success = 0;
	    break;
	  }
	/* fall through */
      /* An explicit type, such as "6mytype" or "7integer" */
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
	APPEND_BLANK (result);
	if (!demangle_class_name (work, mangled, result)) {
	  --result->p;
	  success = 0;
	}
	break;
      case 't':
        success = demangle_template(work,mangled, result, 0);
        break;
      default:
	success = 0;
	break;
      }

  return (success);
}

/* `result' will be initialized in do_type; it will be freed on failure */

static int
do_arg (work, mangled, result)
     struct work_stuff *work;
     const char **mangled;
     string *result;
{
  const char *start = *mangled;

  if (!do_type (work, mangled, result))
    {
      return (0);
    }
  else
    {
      remember_type (work, start, *mangled - start);
      return (1);
    }
}

static void
remember_type (work, start, len)
     struct work_stuff *work;
     const char *start;
     int len;
{
  char *tem;

  if (work -> ntypes >= work -> typevec_size)
    {
      if (work -> typevec_size == 0)
	{
	  work -> typevec_size = 3;
	  work -> typevec =
	    (char **) xmalloc (sizeof (char *) * work -> typevec_size);
	}
      else
	{
	  work -> typevec_size *= 2;
	  work -> typevec =
	    (char **) xrealloc ((char *)work -> typevec,
				sizeof (char *) * work -> typevec_size);
	}
    }
  tem = xmalloc (len + 1);
  memcpy (tem, start, len);
  tem[len] = '\0';
  work -> typevec[work -> ntypes++] = tem;
}

/* Forget the remembered types, but not the type vector itself. */

static void
forget_types (work)
     struct work_stuff *work;
{
  int i;

  while (work -> ntypes > 0)
    {
      i = --(work -> ntypes);
      if (work -> typevec[i] != NULL)
	{
	  free (work -> typevec[i]);
	  work -> typevec[i] = NULL;
	}
    }
}

/* Process the argument list part of the signature, after any class spec
   has been consumed, as well as the first 'F' character (if any).  For
   example:

   "__als__3fooRT0"		=>	process "RT0"
   "complexfunc5__FPFPc_PFl_i"	=>	process "PFPc_PFl_i"

   DECLP must be already initialised, usually non-empty.  It won't be freed
   on failure.

   Note that g++ differs significantly from ARM and lucid style mangling
   with regards to references to previously seen types.  For example, given
   the source fragment:

     class foo {
       public:
       foo::foo (int, foo &ia, int, foo &ib, int, foo &ic);
     };

     foo::foo (int, foo &ia, int, foo &ib, int, foo &ic) { ia = ib = ic; }
     void foo (int, foo &ia, int, foo &ib, int, foo &ic) { ia = ib = ic; }

   g++ produces the names:

     __3fooiRT0iT2iT2
     foo__FiR3fooiT1iT1

   while lcc (and presumably other ARM style compilers as well) produces:

     foo__FiR3fooT1T2T1T2
     __ct__3fooFiR3fooT1T2T1T2

   Note that g++ bases it's type numbers starting at zero and counts all
   previously seen types, while lucid/ARM bases it's type numbers starting
   at one and only considers types after it has seen the 'F' character
   indicating the start of the function args.  For lucid/ARM style, we
   account for this difference by discarding any previously seen types when
   we see the 'F' character, and subtracting one from the type number
   reference.

 */

static int
demangle_args (work, mangled, declp)
     struct work_stuff *work;
     const char **mangled;
     string *declp;
{
  string arg;
  int need_comma = 0;
  int r;
  int t;
  const char *tem;
  char temptype;

  if (PRINT_ARG_TYPES)
    {
      string_append (declp, "(");
      if (**mangled == '\0')
	{
	  string_append (declp, "void");
	}
    }

  while (**mangled != '_' && **mangled != '\0' && **mangled != 'e')
    {
      if ((**mangled == 'N') || (**mangled == 'T'))
	{
	  temptype = *(*mangled)++;
	  
	  if (temptype == 'N')
	    {
	      if (!get_count (mangled, &r))
		{
		  return (0);
		}
	    }
	  else
	    {
	      r = 1;
	    }
          if (ARM_DEMANGLING && work -> ntypes >= 10)
            {
              /* If we have 10 or more types we might have more than a 1 digit
                 index so we'll have to consume the whole count here. This
                 will lose if the next thing is a type name preceded by a
                 count but it's impossible to demangle that case properly
                 anyway. Eg if we already have 12 types is T12Pc "(..., type1,
                 Pc, ...)"  or "(..., type12, char *, ...)" */
              if ((t = consume_count(mangled)) == 0)
                {
                  return (0);
                }
            }
          else
	    {
	      if (!get_count (mangled, &t))
	    	{
	          return (0);
	    	}
	    }
	  if (LUCID_DEMANGLING || ARM_DEMANGLING)
	    {
	      t--;
	    }
	  /* Validate the type index.  Protect against illegal indices from
	     malformed type strings. */
	  if ((t < 0) || (t >= work -> ntypes))
	    {
	      return (0);
	    }
	  while (--r >= 0)
	    {
	      tem = work -> typevec[t];
	      if (need_comma && PRINT_ARG_TYPES)
		{
		  string_append (declp, ", ");
		}
	      if (!do_arg (work, &tem, &arg))
		{
		  return (0);
		}
	      if (PRINT_ARG_TYPES)
		{
		  string_appends (declp, &arg);
		}
	      string_delete (&arg);
	      need_comma = 1;
	    }
	}
      else
	{
	  if (need_comma & PRINT_ARG_TYPES)
	    {
	      string_append (declp, ", ");
	    }
	  if (!do_arg (work, mangled, &arg))
	    {
	      return (0);
	    }
	  if (PRINT_ARG_TYPES)
	    {
	      string_appends (declp, &arg);
	    }
	  string_delete (&arg);
	  need_comma = 1;
	}
    }

  if (**mangled == 'e')
    {
      (*mangled)++;
      if (PRINT_ARG_TYPES)
	{
	  if (need_comma)
	    {
	      string_append (declp, ",");
	    }
	  string_append (declp, "...");
	}
    }

  if (PRINT_ARG_TYPES)
    {
      string_append (declp, ")");
    }
  return (1);
}

static void
demangle_function_name (work, mangled, declp, scan)
     struct work_stuff *work;
     const char **mangled;
     string *declp;
     const char *scan;
{
  int i;
  int len;
  string type;
  const char *tem;

  string_appendn (declp, (*mangled), scan - (*mangled));
  string_need (declp, 1);
  *(declp -> p) = '\0';

  /* Consume the function name, including the "__" separating the name
     from the signature.  We are guaranteed that SCAN points to the
     separator. */

  (*mangled) = scan + 2;

  if (LUCID_DEMANGLING || ARM_DEMANGLING)
    {

      /* See if we have an ARM style constructor or destructor operator.
	 If so, then just record it, clear the decl, and return.
	 We can't build the actual constructor/destructor decl until later,
	 when we recover the class name from the signature. */

      if (strcmp (declp -> b, "__ct") == 0)
	{
	  work -> constructor += 1;
	  string_clear (declp);
	  return;
	}
      else if (strcmp (declp -> b, "__dt") == 0)
	{
	  work -> destructor += 1;
	  string_clear (declp);
	  return;
	}
    }

  if (declp->p - declp->b >= 3 
      && declp->b[0] == 'o'
      && declp->b[1] == 'p'
      && strchr (cplus_markers, declp->b[2]) != NULL)
    {
      /* see if it's an assignment expression */
      if (declp->p - declp->b >= 10 /* op$assign_ */
	  && memcmp (declp->b + 3, "assign_", 7) == 0)
	{
	  for (i = 0; i < sizeof (optable) / sizeof (optable[0]); i++)
	    {
	      len = declp->p - declp->b - 10;
	      if (strlen (optable[i].in) == len
		  && memcmp (optable[i].in, declp->b + 10, len) == 0)
		{
		  string_clear (declp);
		  string_append (declp, "operator");
		  string_append (declp, optable[i].out);
		  string_append (declp, "=");
		  break;
		}
	    }
	}
      else
	{
	  for (i = 0; i < sizeof (optable) / sizeof (optable[0]); i++)
	    {
	      int len = declp->p - declp->b - 3;
	      if (strlen (optable[i].in) == len 
		  && memcmp (optable[i].in, declp->b + 3, len) == 0)
		{
		  string_clear (declp);
		  string_append (declp, "operator");
		  string_append (declp, optable[i].out);
		  break;
		}
	    }
	}
    }
  else if (declp->p - declp->b >= 5 && memcmp (declp->b, "type", 4) == 0
	   && strchr (cplus_markers, declp->b[4]) != NULL)
    {
      /* type conversion operator */
      tem = declp->b + 5;
      if (do_type (work, &tem, &type))
	{
	  string_clear (declp);
	  string_append (declp, "operator ");
	  string_appends (declp, &type);
	  string_delete (&type);
	}
    }
  else if (declp->b[0] == '_' && declp->b[1] == '_'
	  && declp->b[2] == 'o' && declp->b[3] == 'p')
    {
      /* ANSI.  */
      /* type conversion operator.  */
      tem = declp->b + 4;
      if (do_type (work, &tem, &type))
	{
	  string_clear (declp);
	  string_append (declp, "operator ");
	  string_appends (declp, &type);
	  string_delete (&type);
	}
    }
  else if (declp->b[0] == '_' && declp->b[1] == '_'
	   && declp->b[2] >= 'a' && declp->b[2] <= 'z'
	   && declp->b[3] >= 'a' && declp->b[3] <= 'z')
    {
      if (declp->b[4] == '\0')
	{
	  /* Operator.  */
	  for (i = 0; i < sizeof (optable) / sizeof (optable[0]); i++)
	    {
	      if (strlen (optable[i].in) == 2
		  && memcmp (optable[i].in, declp->b + 2, 2) == 0)
		{
		  string_clear (declp);
		  string_append (declp, "operator");
		  string_append (declp, optable[i].out);
		  break;
		}
	    }
	}
      else
	{
	  if (declp->b[2] == 'a' && declp->b[5] == '\0')
	    {
	      /* Assignment. */
	      for (i = 0; i < sizeof (optable) / sizeof (optable[0]); i++)
		{
		  if (strlen (optable[i].in) == 3
		      && memcmp (optable[i].in, declp->b + 2, 3) == 0)
		    {
		      string_clear (declp);
		      string_append (declp, "operator");
		      string_append (declp, optable[i].out);
		      break;
		    }		      
		}
	    }
	}
    }
}

/* a mini string-handling package */

static void
string_need (s, n)
     string *s;
     int n;
{
  int tem;

  if (s->b == NULL)
    {
      if (n < 32)
	{
	  n = 32;
	}
      s->p = s->b = xmalloc (n);
      s->e = s->b + n;
    }
  else if (s->e - s->p < n)
    {
      tem = s->p - s->b;
      n += tem;
      n *= 2;
      s->b = xrealloc (s->b, n);
      s->p = s->b + tem;
      s->e = s->b + n;
    }
}

static void
string_delete (s)
     string *s;
{
  if (s->b != NULL)
    {
      free (s->b);
      s->b = s->e = s->p = NULL;
    }
}

static void
string_init (s)
     string *s;
{
  s->b = s->p = s->e = NULL;
}

static void 
string_clear (s)
     string *s;
{
  s->p = s->b;
}

#if 0

static int
string_empty (s)
     string *s;
{
  return (s->b == s->p);
}

#endif

static void
string_append (p, s)
     string *p;
     const char *s;
{
  int n;
  if (s == NULL || *s == '\0')
    return;
  n = strlen (s);
  string_need (p, n);
  memcpy (p->p, s, n);
  p->p += n;
}

static void
string_appends (p, s)
     string *p, *s;
{
  int n;

  if (s->b != s->p)
    {
      n = s->p - s->b;
      string_need (p, n);
      memcpy (p->p, s->b, n);
      p->p += n;
    }
}

static void
string_appendn (p, s, n)
     string *p;
     const char *s;
     int n;
{
  if (n != 0)
    {
      string_need (p, n);
      memcpy (p->p, s, n);
      p->p += n;
    }
}

static void
string_prepend (p, s)
     string *p;
     const char *s;
{
  if (s != NULL && *s != '\0')
    {
      string_prependn (p, s, strlen (s));
    }
}

static void
string_prepends (p, s)
     string *p, *s;
{
  if (s->b != s->p)
    {
      string_prependn (p, s->b, s->p - s->b);
    }
}

static void
string_prependn (p, s, n)
     string *p;
     const char *s;
     int n;
{
  char *q;

  if (n != 0)
    {
      string_need (p, n);
      for (q = p->p - 1; q >= p->b; q--)
	{
	  q[n] = q[0];
	}
      memcpy (p->b, s, n);
      p->p += n;
    }
}

/* To generate a standalone demangler program for testing purposes,
   just compile and link this file with -DMAIN and libiberty.a.  When
   run, it demangles each command line arg, or each stdin string, and
   prints the result on stdout. */

#ifdef MAIN

static void
demangle_it (mangled_name)
  char *mangled_name;
{
  char *result;

  result = cplus_demangle (mangled_name, DMGL_PARAMS | DMGL_ANSI);
  if (result == NULL)
    {
      printf ("%s\n", mangled_name);
    }
  else
    {
      printf ("%s\n", result);
      free (result);
    }
}

#include "getopt.h"

static char *program_name;
static char *program_version = VERSION;

static void
usage (stream, status)
     FILE *stream;
     int status;
{    
  fprintf (stream, "\
Usage: %s [-_] [-n] [-s {gnu,lucid,arm}] [--strip-underscores]\n\
       [--no-strip-underscores] [--format={gnu,lucid,arm}]\n\
       [--help] [--version] [arg...]\n",
	   program_name);
  exit (status);
}

#define MBUF_SIZE 512
char mbuffer[MBUF_SIZE];

/* Defined in the automatically-generated underscore.c. */
extern int prepends_underscore;

int strip_underscore = 0;

static struct option long_options[] = {
  {"strip-underscores", no_argument, 0, '_'},
  {"format", required_argument, 0, 's'},
  {"help", no_argument, 0, 'h'},
  {"no-strip-underscores", no_argument, 0, 'n'},
  {"version", no_argument, 0, 'v'},
  {0, no_argument, 0, 0}
};

int
main (argc, argv)
     int argc;
     char **argv;
{
  char *result;
  int c;

  program_name = argv[0];

  strip_underscore = prepends_underscore;

  while ((c = getopt_long (argc, argv, "_ns:", long_options, (int *) 0)) != EOF)
    {
      switch (c)
	{
	  case '?':
	    usage (stderr, 1);
	    break;
	  case 'h':
	    usage (stdout, 0);
	  case 'n':
	    strip_underscore = 0;
	    break;
	  case 'v':
	    printf ("GNU %s version %s\n", program_name, program_version);
	    exit (0);
	  case '_':
	    strip_underscore = 1;
	    break;
	  case 's':
	    if (strcmp (optarg, "gnu") == 0)
	      {
		current_demangling_style = gnu_demangling;
	      }
	    else if (strcmp (optarg, "lucid") == 0)
	      {
		current_demangling_style = lucid_demangling;
	      }
	    else if (strcmp (optarg, "arm") == 0)
	      {
		current_demangling_style = arm_demangling;
	      }
	    else
	      {
		fprintf (stderr, "%s: unknown demangling style `%s'\n",
			 program_name, optarg);
		exit (1);
	      }
	    break;
	}
    }

  if (optind < argc)
    {
      for ( ; optind < argc; optind++)
	{
	  demangle_it (argv[optind]);
	}
    }
  else
    {
      for (;;)
	{
	  int i = 0;
	  c = getchar ();
	  /* Try to read a label. */
	  while (c != EOF && (isalnum(c) || c == '_' || c == '$' || c == '.'))
	    {
	      if (i >= MBUF_SIZE-1)
		break;
	      mbuffer[i++] = c;
	      c = getchar ();
	    }
	  if (i > 0)
	    {
	      int skip_first = 0;

	      if (mbuffer[0] == '.')
		++skip_first;
	      if (strip_underscore && mbuffer[skip_first] == '_')
		++skip_first;

	      if (skip_first > i)
		skip_first = i;

	      mbuffer[i] = 0;
	      
	      result = cplus_demangle (mbuffer + skip_first,
				       DMGL_PARAMS | DMGL_ANSI);
	      if (result)
		{
		  if (mbuffer[0] == '.')
		    putc ('.', stdout);
		  fputs (result, stdout);
		  free (result);
		}
	      else
		fputs (mbuffer, stdout);

	      fflush (stdout);
	    }
	  if (c == EOF)
	    break;
	  putchar (c);
	}
    }

  exit (0);
}

static void
fatal (str)
     char *str;
{
  fprintf (stderr, "%s: %s\n", program_name, str);
  exit (1);
}

char * malloc ();
char * realloc ();

char *
xmalloc (size)
     unsigned size;
{
  register char *value = (char *) malloc (size);
  if (value == 0)
    fatal ("virtual memory exhausted");
  return value;
}

char *
xrealloc (ptr, size)
     char *ptr;
     unsigned size;
{
  register char *value = (char *) realloc (ptr, size);
  if (value == 0)
    fatal ("virtual memory exhausted");
  return value;
}
#endif	/* main */
