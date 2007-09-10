// { dg-do compile }
// { dg-options "-O2" }

int bar (int, const char *, int, ...);
int baz (int, const char *, long int);

extern inline __attribute__((always_inline)) int
f2 (int y, ...)
{
  return bar (y, "", __builtin_va_arg_pack ());		/* { dg-error "invalid use of" } */
}

extern inline __attribute__((always_inline)) int
f3 (int y, ...)
{
  return bar (y, "", 5, __builtin_va_arg_pack ());
}

extern inline __attribute__((always_inline)) int
f4 (int y, ...)
{
  return bar (y, "", 4, __builtin_va_arg_pack (), 6);	/* { dg-error "invalid use of" } */
}

extern inline __attribute__((always_inline)) int
f5 (int y, ...)
{
  return baz (y, "", __builtin_va_arg_pack ());		/* { dg-error "invalid use of" } */
}

extern inline __attribute__((always_inline)) int
f6 (int y, ...)
{
  return __builtin_va_arg_pack ();			/* { dg-error "invalid use of" } */
}

int
test (void)
{
  int a = f2 (5, "a", 6);
  a += f3 (6, "ab", 17LL);
  a += f4 (7, 1, 2, 3);
  a += f5 (8, 7L);
  a += f6 (9);
  return a;
}
