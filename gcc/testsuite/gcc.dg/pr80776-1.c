/* { dg-do compile } */
/* { dg-options "-O2 -Wformat-overflow" } */

extern __inline __attribute__ ((__always_inline__)) __attribute__ ((__gnu_inline__)) __attribute__ ((__artificial__)) int
__attribute__ ((__nothrow__ , __leaf__)) sprintf (char *__restrict __s, const char *__restrict __fmt, ...)
{
  return __builtin___sprintf_chk (__s, 2 - 1,
				  __builtin_object_size (__s, 2 > 1), __fmt, __builtin_va_arg_pack ());
}
char number[sizeof "999999"];
int somerandom (void);
void
Foo (void)
{
  int i = somerandom ();
  if (! (0 <= i))
    __builtin_unreachable ();
  if (! (0 <= i && i <= 999999))
    __builtin_unreachable ();
  /* The correctness bits for [E]VRP cannot handle chained conditionals
     when deciding to ignore a unreachable branch for setting SSA range info. */
  sprintf (number, "%d", i); /* { dg-bogus "writing" "" { xfail *-*-* } } */
}
