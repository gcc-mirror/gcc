/* Header describing internals of libintl library.
   Copyright (C) 1995-1999, 2000, 2001 Free Software Foundation, Inc.
   Written by Ulrich Drepper <drepper@cygnus.com>, 1995.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License as published
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
   USA.  */

#ifndef _GETTEXTP_H
#define _GETTEXTP_H

#include <stddef.h>		/* Get size_t.  */

#ifdef _LIBC
# include "../iconv/gconv_int.h"
#else
# if HAVE_ICONV
#  include <iconv.h>
# endif
#endif

#include "loadinfo.h"

#include "gettext.h"		/* Get nls_uint32.  */

/* @@ end of prolog @@ */

#ifndef PARAMS
# if __STDC__
#  define PARAMS(args) args
# else
#  define PARAMS(args) ()
# endif
#endif

#ifndef internal_function
# define internal_function
#endif

/* Tell the compiler when a conditional or integer expression is
   almost always true or almost always false.  */
#ifndef HAVE_BUILTIN_EXPECT
# define __builtin_expect(expr, val) (expr)
#endif

#ifndef W
# define W(flag, data) ((flag) ? SWAP (data) : (data))
#endif


#ifdef _LIBC
# include <byteswap.h>
# define SWAP(i) bswap_32 (i)
#else
static inline nls_uint32
SWAP (i)
     nls_uint32 i;
{
  return (i << 24) | ((i & 0xff00) << 8) | ((i >> 8) & 0xff00) | (i >> 24);
}
#endif


/* This is the representation of the expressions to determine the
   plural form.  */
struct expression
{
  int nargs;			/* Number of arguments.  */
  enum operator
  {
    /* Without arguments:  */
    var,			/* The variable "n".  */
    num,			/* Decimal number.  */
    /* Unary operators:  */
    lnot,			/* Logical NOT.  */
    /* Binary operators:  */
    mult,			/* Multiplication.  */
    divide,			/* Division.  */
    module,			/* Module operation.  */
    plus,			/* Addition.  */
    minus,			/* Subtraction.  */
    less_than,			/* Comparison.  */
    greater_than,		/* Comparison.  */
    less_or_equal,		/* Comparison.  */
    greater_or_equal,		/* Comparison.  */
    equal,			/* Comparision for equality.  */
    not_equal,			/* Comparision for inequality.  */
    land,			/* Logical AND.  */
    lor,			/* Logical OR.  */
    /* Ternary operators:  */
    qmop			/* Question mark operator.  */
  } operation;
  union
  {
    unsigned long int num;	/* Number value for `num'.  */
    struct expression *args[3];	/* Up to three arguments.  */
  } val;
};

/* This is the data structure to pass information to the parser and get
   the result in a thread-safe way.  */
struct parse_args
{
  const char *cp;
  struct expression *res;
};


/* The representation of an opened message catalog.  */
struct loaded_domain
{
  const char *data;
  int use_mmap;
  size_t mmap_size;
  int must_swap;
  nls_uint32 nstrings;
  struct string_desc *orig_tab;
  struct string_desc *trans_tab;
  nls_uint32 hash_size;
  nls_uint32 *hash_tab;
  int codeset_cntr;
#ifdef _LIBC
  __gconv_t conv;
#else
# if HAVE_ICONV
  iconv_t conv;
# endif
#endif
  char **conv_tab;

  struct expression *plural;
  unsigned long int nplurals;
};

/* We want to allocate a string at the end of the struct.  But ISO C
   doesn't allow zero sized arrays.  */
#ifdef __GNUC__
# define ZERO 0
#else
# define ZERO 1
#endif

/* A set of settings bound to a message domain.  Used to store settings
   from bindtextdomain() and bind_textdomain_codeset().  */
struct binding
{
  struct binding *next;
  char *dirname;
  int codeset_cntr;	/* Incremented each time codeset changes.  */
  char *codeset;
  char domainname[ZERO];
};

/* A counter which is incremented each time some previous translations
   become invalid.
   This variable is part of the external ABI of the GNU libintl.  */
extern int _nl_msg_cat_cntr;

struct loaded_l10nfile *_nl_find_domain PARAMS ((const char *__dirname,
						 char *__locale,
						 const char *__domainname,
					      struct binding *__domainbinding))
     internal_function;
void _nl_load_domain PARAMS ((struct loaded_l10nfile *__domain,
			      struct binding *__domainbinding))
     internal_function;
void _nl_unload_domain PARAMS ((struct loaded_domain *__domain))
     internal_function;
const char *_nl_init_domain_conv PARAMS ((struct loaded_l10nfile *__domain_file,
					  struct loaded_domain *__domain,
					  struct binding *__domainbinding))
     internal_function;
void _nl_free_domain_conv PARAMS ((struct loaded_domain *__domain))
     internal_function;

char *_nl_find_msg PARAMS ((struct loaded_l10nfile *domain_file,
			    struct binding *domainbinding,
			    const char *msgid, size_t *lengthp))
     internal_function;

#ifdef _LIBC
extern char *__gettext PARAMS ((const char *__msgid));
extern char *__dgettext PARAMS ((const char *__domainname,
				 const char *__msgid));
extern char *__dcgettext PARAMS ((const char *__domainname,
				  const char *__msgid, int __category));
extern char *__ngettext PARAMS ((const char *__msgid1, const char *__msgid2,
				 unsigned long int __n));
extern char *__dngettext PARAMS ((const char *__domainname,
				  const char *__msgid1, const char *__msgid2,
				  unsigned long int n));
extern char *__dcngettext PARAMS ((const char *__domainname,
				   const char *__msgid1, const char *__msgid2,
				   unsigned long int __n, int __category));
extern char *__dcigettext PARAMS ((const char *__domainname,
				   const char *__msgid1, const char *__msgid2,
				   int __plural, unsigned long int __n,
				   int __category));
extern char *__textdomain PARAMS ((const char *__domainname));
extern char *__bindtextdomain PARAMS ((const char *__domainname,
				       const char *__dirname));
extern char *__bind_textdomain_codeset PARAMS ((const char *__domainname,
						const char *__codeset));
#else
extern char *gettext__ PARAMS ((const char *__msgid));
extern char *dgettext__ PARAMS ((const char *__domainname,
				 const char *__msgid));
extern char *dcgettext__ PARAMS ((const char *__domainname,
				  const char *__msgid, int __category));
extern char *ngettext__ PARAMS ((const char *__msgid1, const char *__msgid2,
				 unsigned long int __n));
extern char *dngettext__ PARAMS ((const char *__domainname,
				  const char *__msgid1, const char *__msgid2,
				  unsigned long int __n));
extern char *dcngettext__ PARAMS ((const char *__domainname,
				   const char *__msgid1, const char *__msgid2,
				   unsigned long int __n, int __category));
extern char *dcigettext__ PARAMS ((const char *__domainname,
				   const char *__msgid1, const char *__msgid2,
				   int __plural, unsigned long int __n,
				   int __category));
extern char *textdomain__ PARAMS ((const char *__domainname));
extern char *bindtextdomain__ PARAMS ((const char *__domainname,
				       const char *__dirname));
extern char *bind_textdomain_codeset__ PARAMS ((const char *__domainname,
						const char *__codeset));
#endif

#ifdef _LIBC
extern void __gettext_free_exp PARAMS ((struct expression *exp))
     internal_function;
extern int __gettextparse PARAMS ((void *arg));
#else
extern void gettext_free_exp__ PARAMS ((struct expression *exp))
     internal_function;
extern int gettextparse__ PARAMS ((void *arg));
#endif

/* @@ begin of epilog @@ */

#endif /* gettextP.h  */
