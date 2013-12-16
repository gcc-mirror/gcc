/* PR middle-end/58956 */
/* PR middle-end/59470 */
/* { dg-do run } */
/* { dg-options "-O2" } */

int a, b, d[1024];

int
main ()
{
  int c = a;
  asm ("{movl $6, (%2); movl $1, %0|mov dword ptr [%2], 6; mov %0, 1}"
       : "=r" (d[c]) : "rm" (b), "r" (&a) : "memory");
  if (d[0] != 1 || d[6] != 0)
    __builtin_abort ();
  return 0;
}
