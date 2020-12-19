/* PR middle-end/98366 */
/* { dg-require-effective-target int32 } */

typedef struct S { int a, b, c : 7, d : 8, e : 17; } S;
const S f[] = { {0, 3, 4, 2, 0} };

int
main ()
{
  if (__builtin_memcmp (f, (S[]){{.b = 3, .c = 4, .d = 2, .e = 0}}, sizeof (S)))
    __builtin_abort ();
  return 0;
}
