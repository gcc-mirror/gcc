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

  /* Legacy evrp sets the range of i to [0, MAX] *before* the first conditional,
     and to [0,999999] *before* the second conditional.  This is because both
     evrp and VRP use trickery to set global ranges when this particular use of
     a __builtin_unreachable is in play (see uses of
     assert_unreachable_fallthru_edge_p).

     Setting these ranges at the definition site, causes VRP to remove the
     unreachable code altogether, leaving the following sprintf unguarded.  This
     causes the bogus warning below.  */
  sprintf (number, "%d", i); /* { dg-bogus "writing" "" } */
}
