/* PR tree-optimization/90892 */
/* { dg-do run } */
/* { dg-options "-O2" } */

const char *a = "A\0b";

int
main()
{
  if (__builtin_strncmp(a, "A\0", 2) != 0)
    __builtin_abort ();

  return 0;
}
