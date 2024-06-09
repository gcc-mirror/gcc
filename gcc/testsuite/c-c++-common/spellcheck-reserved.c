/* Verify that we don't offer names that are reserved for the
   implementation (PR c/83236).  */
/* { dg-options "-nostdinc" } */

/* Example of an identifier with a leading double-underscore.
   We shouldn't offer '__ino_t' as a suggestion for an unknown 'ino_t'.  */

typedef unsigned long int __ino_t;
ino_t inode; /* { dg-error "did you mean 'int'" } */


/* Example of a typedef with a leading double-underscore.  */

typedef unsigned char __u_char;
u_char ch; /* { dg-error "did you mean 'char'" } */


/* Example of a macro with a leading double-underscore.  */

# define __SOME_MACRO	int

SOME_MACRO foo; /* { dg-bogus "__SOME_MACRO" } */
/* { dg-error "'SOME_MACRO'" "" { target *-*-* } .-1 } */


/* If the misspelled name itself matches the "reserved" pattern, then
   it's OK to suggest such names.  */

void test (const char *buf, char ch)
{
  __builtin_strtchr (buf, ch); /* { dg-line misspelled_reserved } */
  /* { dg-error "did you mean '__builtin_strchr'" "" { target c } misspelled_reserved } */
  /* { dg-error "'__builtin_strtchr' was not declared in this scope; did you mean '__builtin_strrchr'\\?" "" { target c++ } misspelled_reserved } */
}

/* Similarly for a name that begins with a single underscore.  */

void test_2 (const char *buf, char ch)
{
  _builtin_strchr (buf, ch); /* { dg-line misspelled_one_underscore } */
  /* { dg-error "did you mean '__builtin_strchr'" "" { target c } misspelled_one_underscore } */
  /* { dg-error "'_builtin_strchr' was not declared in this scope; did you mean '__builtin_strchr'\\?" "" { target c++ } misspelled_one_underscore } */
}

/* Verify that we can correct "__FILE_" to "__FILE__".  */

const char * test_3 (void)
{
  return __FILE_; /* { dg-line misspelled__FILE_ } */
  /* { dg-error "did you mean '__FILE__'" "" { target c } misspelled__FILE_ } */
  /* { dg-error "'__FILE_' was not declared in this scope; did you mean '__FILE__'\\?"  "" { target c++ } misspelled__FILE_ } */
}

/* Verify that we can correct "__FILE_NAME_" to "__FILE_NAME__".  */

const char * test_4 (void)
{
  return __FILE_NAME_; /* { dg-line misspelled__FILE_NAME_ } */
  /* { dg-error "did you mean '__FILE_NAME__'" "" { target c } misspelled__FILE_NAME_ } */
  /* { dg-error "'__FILE_NAME_' was not declared in this scope; did you mean '__FILE_NAME__'\\?"  "" { target c++ } misspelled__FILE_NAME_ } */
}

/* Verify that we can correct "__FILENAME__" to "__FILE_NAME__".  */

const char * test_5 (void)
{
  return __FILENAME__; /* { dg-line misspelled__FILENAME__ } */
  /* { dg-error "did you mean '__FILE_NAME__'" "" { target c } misspelled__FILENAME__ } */
  /* { dg-error "'__FILENAME__' was not declared in this scope; did you mean '__FILE_NAME__'\\?"  "" { target c++ } misspelled__FILENAME__ } */
}
