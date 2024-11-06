/* { dg-do run } */
/* { dg-options "-std=c23" } */

int
foo (int x)
{
  if (x != 3)
    __builtin_abort ();
  return 1;
}

int
main ()
{
  unsigned char a[] = {
    [5] = foo (0),
    [7] = foo (1),
    [42] = foo (2),
    #embed __FILE__ prefix([0] = ) suffix (,) /* { dg-warning "initialized field with side-effects overwritten" } */
    [12] = foo (3) /* { dg-message "near initialization" "" { target *-*-* } .-1 } */

  };
  const unsigned char b[] = {
    #embed __FILE__
  };
  if (sizeof (a) != sizeof (b)
      || __builtin_memcmp (a, b, 12)
      || a[12] != 1
      || __builtin_memcmp (a + 13, b + 13, sizeof (a) - 13))
    __builtin_abort ();
}
