/* PR tree-optimization/102124 */
/* { dg-do run } */

int
foo (const unsigned char *a, const unsigned char *b, unsigned long len)
{
  int ab, ba; 
  unsigned long i;
  for (i = 0, ab = 0, ba = 0; i < len; i++)
    {
      ab |= a[i] - b[i];
      ba |= b[i] - a[i];
    }   
  return (ab | ba) >= 0;
}

int
main ()
{
  unsigned char a[32] = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' };
  unsigned char b[32] = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a' };
  unsigned char c[32] = { 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b', 'b' };
  if (!foo (a, b, 16))
    __builtin_abort ();
  if (foo (a, c, 16))
    __builtin_abort ();
  return 0;
}
