/* PR c/4308
   This testcase failed because 0x8000000000000000 >> 0
   was incorrectly folded into 0xffffffff00000000.  */

extern void abort (void);
extern void exit (int);

long long foo (void)
{
  long long C = 1ULL << 63, X;
  int Y = 32;
  X = C >> (Y & 31);
  return X;
}

int main (void)
{
  if (foo () != 1ULL << 63)
    abort ();
  exit (0);
}
