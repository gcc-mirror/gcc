/* See e.g. https://en.cppreference.com/w/c/string/byte/strcat */

#include "analyzer-decls.h"

char *strcat (char *dest, const char *src);
#define NULL ((void *)0)

char *
test_passthrough (char *dest, const char *src)
{
  return strcat (dest, src);
}

char *
test_null_dest (const char *src)
{
  return strcat (NULL, src); /* { dg-warning "use of NULL where non-null expected" } */
}

char *
test_null_src (char *dest)
{
  return strcat (dest, NULL); /* { dg-warning "use of NULL where non-null expected" } */
}

char *
test_uninit_dest (const char *src)
{
  char dest[10];
  return strcat (dest, src); /* { dg-warning "use of uninitialized value 'dest\\\[0\\\]'" } */
}

char *
test_uninit_src (char *dest)
{
  const char src[10];
  return strcat (dest, src); /* { dg-warning "use of uninitialized value 'src\\\[0\\\]'" } */
}

char *
test_dest_not_terminated (char *src)
{
  char dest[3] = "foo";
  return strcat (dest, src); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 1 \\('&dest'\\) of 'strcat'" "" { target *-*-* } .-1 } */
}

char *
test_src_not_terminated (char *dest)
{
  const char src[3] = "foo";
  return strcat (dest, src); /* { dg-warning "stack-based buffer over-read" } */
  /* { dg-message "while looking for null terminator for argument 2 \\('&src'\\) of 'strcat'" "" { target *-*-* } .-1 } */
}

char * __attribute__((noinline))
call_strcat (char *dest, const char *src)
{
  return strcat (dest, src);
}

void
test_concrete_valid_static_size (void)
{
  char buf[16];
  char *p1 = __builtin_strcpy (buf, "abc");
  char *p2 = call_strcat (buf, "def");
  __analyzer_eval (p1 == buf); /* { dg-warning "TRUE" } */
  __analyzer_eval (p2 == buf); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[0] == 'a'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[1] == 'b'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[2] == 'c'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[3] == 'd'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[4] == 'e'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[5] == 'f'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[6] == '\0'); /* { dg-warning "TRUE" } */
  __analyzer_eval (__builtin_strlen (buf) == 6);  /* { dg-warning "TRUE" } */
}

void
test_concrete_valid_static_size_2 (void)
{
  char buf[16];
  char *p1 = __builtin_strcpy (buf, "abc");
  char *p2 = call_strcat (buf, "def");
  char *p3 = call_strcat (buf, "ghi");
  __analyzer_eval (p1 == buf); /* { dg-warning "TRUE" } */
  __analyzer_eval (p2 == buf); /* { dg-warning "TRUE" } */
  __analyzer_eval (p3 == buf); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[0] == 'a'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[1] == 'b'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[2] == 'c'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[3] == 'd'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[4] == 'e'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[5] == 'f'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[6] == 'g'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[7] == 'h'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[8] == 'i'); /* { dg-warning "TRUE" } */
  __analyzer_eval (buf[9] == '\0'); /* { dg-warning "TRUE" } */
  __analyzer_eval (__builtin_strlen (buf) == 9);  /* { dg-warning "TRUE" } */
  __analyzer_eval (__builtin_strlen (buf + 1) == 8);  /* { dg-warning "TRUE" } */
  __analyzer_eval (__builtin_strlen (buf + 2) == 7);  /* { dg-warning "TRUE" } */
  __analyzer_eval (__builtin_strlen (buf + 3) == 6);  /* { dg-warning "TRUE" } */
  __analyzer_eval (__builtin_strlen (buf + 4) == 5);  /* { dg-warning "TRUE" } */
  __analyzer_eval (__builtin_strlen (buf + 5) == 4);  /* { dg-warning "TRUE" } */
  __analyzer_eval (__builtin_strlen (buf + 6) == 3);  /* { dg-warning "TRUE" } */
  __analyzer_eval (__builtin_strlen (buf + 7) == 2);  /* { dg-warning "TRUE" } */
  __analyzer_eval (__builtin_strlen (buf + 8) == 1);  /* { dg-warning "TRUE" } */
  __analyzer_eval (__builtin_strlen (buf + 9) == 0);  /* { dg-warning "TRUE" } */
}

char * __attribute__((noinline))
call_strcat_invalid (char *dest, const char *src)
{
  return strcat (dest, src); /* { dg-warning "stack-based buffer overflow" } */
}

void
test_concrete_invalid_static_size (void)
{
  char buf[3];
  buf[0] = '\0';
  call_strcat_invalid (buf, "abc");
}

void
test_concrete_symbolic (const char *suffix)
{
  char buf[10];
  buf[0] = '\0';
  call_strcat (buf, suffix);
}

/* TODO:
     - "The behavior is undefined if the strings overlap."
*/
