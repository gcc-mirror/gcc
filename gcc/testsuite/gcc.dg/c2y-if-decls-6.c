/* N3356 - if declarations.  */
/* PR c/117019 */
/* { dg-do run } */
/* { dg-options "-std=c2y -Wall -Wextra" } */
/* Test VLAs.  */

void foo (int) { }

int
main ()
{
  int i = 3;

  if (int arr[i] = { }; !arr[0])
    foo (arr[0]);
  else
    __builtin_abort ();

  switch (int arr[i] = { }; arr[0])
    {
    case 0:
      foo (arr[0]);
      break;
    default:
      __builtin_abort ();
    }
}
