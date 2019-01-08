/* PR middle-end/82564 */
/* { dg-require-effective-target alloca } */

int
main ()
{
  int t = 8, i;
  typedef struct { char v[t]; } B; 
  B a, b;
  B __attribute__ ((noinline)) f () { return b; }
  for (i = 0; i < 8; i++)
    b.v[i] = i;
  a = f ();
  return 0;
}
