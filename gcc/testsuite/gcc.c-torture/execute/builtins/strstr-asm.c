/* Copyright (C) 2000, 2003  Free Software Foundation.

   Ensure all expected transformations of builtin strstr occur and
   perform correctly in presence of redirect.  */
/* { dg-options "-ffat-lto-objects" } */

#define ASMNAME(cname)  ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define ASMNAME2(prefix, cname) STRING (prefix) cname
#define STRING(x)    #x

typedef __SIZE_TYPE__ size_t;
extern void abort (void);
extern char *strstr (const char *, const char *)
  __asm (ASMNAME ("my_strstr"));

const char *p = "rld", *q = "hello world";

void
main_test (void)
{
  const char *const foo = "hello world";

  if (strstr (foo, "") != foo)
    abort ();
  if (strstr (foo + 4, "") != foo + 4)
    abort ();
  if (strstr (foo, "h") != foo)
    abort ();
  if (strstr (foo, "w") != foo + 6)
    abort ();
  if (strstr (foo + 6, "o") != foo + 7)
    abort ();
  if (strstr (foo + 1, "world") != foo + 6)
    abort ();
  if (strstr (foo + 2, p) != foo + 8)
    abort ();
  if (strstr (q, "") != q)
    abort ();
  if (strstr (q + 1, "o") != q + 4)
    abort ();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_strstr (foo + 1, "world") != foo + 6)
    abort ();
}
