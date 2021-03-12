/* Integration test to ensure we issue a FILE * leak diagnostic for
   this particular non-trivial case.
   Adapted from intl/localealias.c, with all #includes removed.  */

/* { dg-do "compile" } */
/* { dg-additional-options "-Wno-analyzer-too-complex" } */
/* TODO: remove the need for this option.  */
/* { dg-require-effective-target alloca } */

/* Handle aliases for locale names.
   Copyright (C) 1995-1999, 2000-2001, 2003 Free Software Foundation, Inc.

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
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301,
   USA.  */

/* Minimal version of system headers.  */

typedef __SIZE_TYPE__ size_t;
#define NULL ((void *) 0)

#define PATH_SEPARATOR ':'
typedef struct _IO_FILE FILE;
extern FILE *fopen(const char *__restrict __filename,
		   const char *__restrict __modes);
extern int feof_unlocked(FILE *__stream) __attribute__((__nothrow__, __leaf__));
extern char *fgets_unlocked(char *__restrict __s, int __n,
			    FILE *__restrict __stream);
extern int fclose(FILE *__stream);

#define alloca __builtin_alloca

extern char *strchr(const char *__s, int __c)
    __attribute__((__nothrow__, __leaf__)) __attribute__((__pure__))
    __attribute__((__nonnull__(1)));
extern void *memcpy(void *__restrict __dest, const void *__restrict __src,
		    size_t __n) __attribute__((__nothrow__, __leaf__))
  __attribute__((__nonnull__(1, 2)));
extern void *mempcpy(void *__restrict __dest, const void *__restrict __src,
		     size_t __n) __attribute__((__nothrow__, __leaf__))
  __attribute__((__nonnull__(1, 2)));
#define HAVE_MEMPCPY 1
extern size_t strlen(const char *__s) __attribute__((__nothrow__, __leaf__))
  __attribute__((__pure__)) __attribute__((__nonnull__(1)));

extern int strcasecmp(const char *__s1, const char *__s2)
    __attribute__((__nothrow__, __leaf__)) __attribute__((__pure__))
    __attribute__((__nonnull__(1, 2)));

extern int isspace(int) __attribute__((__nothrow__, __leaf__));

extern void *realloc(void *__ptr, size_t __size)
    __attribute__((__nothrow__, __leaf__))
    __attribute__((__warn_unused_result__));

typedef int (*__compar_fn_t)(const void *, const void *);
extern void *bsearch(const void *__key, const void *__base, size_t __nmemb,
		     size_t __size, __compar_fn_t __compar)
    __attribute__((__nonnull__(1, 2, 5)));

extern __inline __attribute__((__gnu_inline__)) void *
bsearch(const void *__key, const void *__base, size_t __nmemb, size_t __size,
	__compar_fn_t __compar) {
  size_t __l, __u, __idx;
  const void *__p;
  int __comparison;

  __l = 0;
  __u = __nmemb;
  while (__l < __u) {
    __idx = (__l + __u) / 2;
    __p = (void *)(((const char *)__base) + (__idx * __size));
    __comparison = (*__compar)(__key, __p);
    if (__comparison < 0)
      __u = __idx;
    else if (__comparison > 0)
      __l = __idx + 1;
    else
      return (void *)__p;
  }

  return ((void *)0);
}

extern void qsort(void *__base, size_t __nmemb, size_t __size,
		  __compar_fn_t __compar) __attribute__((__nonnull__(1, 4)));

/* Minimal version of intl headers.  */

#define PARAMS(args) args

#define relocate libintl_relocate
extern const char *libintl_relocate(const char *pathname);

#define LOCALE_ALIAS_PATH "value for LOCALE_ALIAS_PATH"

/* Cleaned-up body of localealias.c follows.  */

#ifndef internal_function
# define internal_function
#endif

/* Some optimizations for glibc.  */
# define FEOF(fp)		feof_unlocked (fp)
# define FGETS(buf, n, fp)	fgets_unlocked (buf, n, fp)

/* For those losing systems which don't have `alloca' we have to add
   some additional code emulating it.  */
# define freea(p) /* nothing */

struct alias_map
{
  const char *alias;
  const char *value;
};

# define libc_freeres_ptr(decl) decl

libc_freeres_ptr (static char *string_space);
static size_t string_space_act;
static size_t string_space_max;
libc_freeres_ptr (static struct alias_map *map);
static size_t nmap;
static size_t maxmap;


/* Prototypes for local functions.  */
static size_t read_alias_file PARAMS ((const char *fname, int fname_len))
     internal_function;
static int extend_alias_table PARAMS ((void));
static int alias_compare PARAMS ((const struct alias_map *map1,
				  const struct alias_map *map2));


const char *
_nl_expand_alias (name)
    const char *name;
{
  static const char *locale_alias_path;
  struct alias_map *retval;
  const char *result = NULL;
  size_t added;

#ifdef _LIBC
  __libc_lock_lock (lock);
#endif

  if (locale_alias_path == NULL)
    locale_alias_path = LOCALE_ALIAS_PATH;

  do
    {
      struct alias_map item;

      item.alias = name;

      if (nmap > 0)
	retval = (struct alias_map *) bsearch (&item, map, nmap,
					       sizeof (struct alias_map),
					       (int (*) PARAMS ((const void *,
								 const void *))
						) alias_compare);
      else
	retval = NULL;

      /* We really found an alias.  Return the value.  */
      if (retval != NULL)
	{
	  result = retval->value;
	  break;
	}

      /* Perhaps we can find another alias file.  */
      added = 0;
      while (added == 0 && locale_alias_path[0] != '\0')
	{
	  const char *start;

	  while (locale_alias_path[0] == PATH_SEPARATOR)
	    ++locale_alias_path;
	  start = locale_alias_path;

	  while (locale_alias_path[0] != '\0'
		 && locale_alias_path[0] != PATH_SEPARATOR)
	    ++locale_alias_path;

	  if (start < locale_alias_path)
	    added = read_alias_file (start, locale_alias_path - start);
	}
    }
  while (added != 0);

#ifdef _LIBC
  __libc_lock_unlock (lock);
#endif

  return result;
}


static size_t
internal_function
read_alias_file (fname, fname_len)
     const char *fname;
     int fname_len;
{
  FILE *fp;
  char *full_fname;
  size_t added;
  static const char aliasfile[] = "/locale.alias";

  full_fname = (char *) alloca (fname_len + sizeof aliasfile);
#ifdef HAVE_MEMPCPY
  mempcpy (mempcpy (full_fname, fname, fname_len),
	   aliasfile, sizeof aliasfile);
#else
  memcpy (full_fname, fname, fname_len);
  memcpy (&full_fname[fname_len], aliasfile, sizeof aliasfile);
#endif

  fp = fopen (relocate (full_fname), "r"); /* { dg-message "opened here" } */
  freea (full_fname);
  if (fp == NULL)
    return 0;

#ifdef HAVE___FSETLOCKING
  /* No threads present.  */
  __fsetlocking (fp, FSETLOCKING_BYCALLER);
#endif

  added = 0;
  while (!FEOF (fp))
    {
      /* It is a reasonable approach to use a fix buffer here because
	 a) we are only interested in the first two fields
	 b) these fields must be usable as file names and so must not
	    be that long
	 We avoid a multi-kilobyte buffer here since this would use up
	 stack space which we might not have if the program ran out of
	 memory.  */
      char buf[400];
      char *alias;
      char *value;
      char *cp;

      if (FGETS (buf, sizeof buf, fp) == NULL)
	/* EOF reached.  */
	break;

      cp = buf;
      /* Ignore leading white space.  */
      while (isspace ((unsigned char) cp[0]))
	++cp;

      /* A leading '#' signals a comment line.  */
      if (cp[0] != '\0' && cp[0] != '#')
	{
	  alias = cp++;
	  while (cp[0] != '\0' && !isspace ((unsigned char) cp[0]))
	    ++cp;
	  /* Terminate alias name.  */
	  if (cp[0] != '\0')
	    *cp++ = '\0';

	  /* Now look for the beginning of the value.  */
	  while (isspace ((unsigned char) cp[0]))
	    ++cp;

	  if (cp[0] != '\0')
	    {
	      size_t alias_len;
	      size_t value_len;

	      value = cp++;
	      while (cp[0] != '\0' && !isspace ((unsigned char) cp[0]))
		++cp;
	      /* Terminate value.  */
	      if (cp[0] == '\n')
		{
		  /* This has to be done to make the following test
		     for the end of line possible.  We are looking for
		     the terminating '\n' which do not overwrite here.  */
		  *cp++ = '\0';
		  *cp = '\n';
		}
	      else if (cp[0] != '\0')
		*cp++ = '\0';

	      if (nmap >= maxmap)
		if (__builtin_expect (extend_alias_table (), 0))
		  return added; /* { dg-warning "leak of FILE 'fp'" } */

	      alias_len = strlen (alias) + 1;
	      value_len = strlen (value) + 1;

	      if (string_space_act + alias_len + value_len > string_space_max)
		{
		  /* Increase size of memory pool.  */
		  size_t new_size = (string_space_max
				     + (alias_len + value_len > 1024
					? alias_len + value_len : 1024));
		  char *new_pool = (char *) realloc (string_space, new_size);
		  if (new_pool == NULL)
		    return added;

		  if (__builtin_expect (string_space != new_pool, 0))
		    {
		      size_t i;

		      for (i = 0; i < nmap; i++)
			{
			  map[i].alias += new_pool - string_space;
			  map[i].value += new_pool - string_space;
			}
		    }

		  string_space = new_pool;
		  string_space_max = new_size;
		}

	      map[nmap].alias = memcpy (&string_space[string_space_act],
					alias, alias_len);
	      string_space_act += alias_len;

	      map[nmap].value = memcpy (&string_space[string_space_act],
					value, value_len);
	      string_space_act += value_len;

	      ++nmap;
	      ++added;
	    }
	}

      /* Possibly not the whole line fits into the buffer.  Ignore
	 the rest of the line.  */
      while (strchr (buf, '\n') == NULL)
	if (FGETS (buf, sizeof buf, fp) == NULL)
	  /* Make sure the inner loop will be left.  The outer loop
	     will exit at the `feof' test.  */
	  break;
    }

  /* Should we test for ferror()?  I think we have to silently ignore
     errors.  --drepper  */
  fclose (fp);

  if (added > 0)
    qsort (map, nmap, sizeof (struct alias_map),
	   (int (*) PARAMS ((const void *, const void *))) alias_compare);

  return added;
}


static int
extend_alias_table ()
{
  size_t new_size;
  struct alias_map *new_map;

  new_size = maxmap == 0 ? 100 : 2 * maxmap;
  new_map = (struct alias_map *) realloc (map, (new_size
						* sizeof (struct alias_map)));
  if (new_map == NULL)
    /* Simply don't extend: we don't have any more core.  */
    return -1;

  map = new_map;
  maxmap = new_size;
  return 0;
}


static int
alias_compare (map1, map2)
     const struct alias_map *map1;
     const struct alias_map *map2;
{
  return strcasecmp (map1->alias, map2->alias);
}
