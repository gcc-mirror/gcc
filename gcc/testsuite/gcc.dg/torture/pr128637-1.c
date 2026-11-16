/* { dg-do run { target { lp64 && le } } } */

void abort (void);
void *malloc(unsigned long);
void *memset(void *, int, unsigned long);

int x;
long r;
long long __attribute__((noipa))
foo (unsigned char *b, unsigned char *w, long long z)
{
  memset (b, 0, 8);
  *(long long *)w = z;
  b[4] = 1;
  if (x)
    r = *(long *)b;
  return *(long long *)b;
}

int main(void)
{
  unsigned char *b = malloc(sizeof (long long));
  long long t = foo (b, b, 1);
  if (t != 0x100000001)
    abort ();
  return 0;
}
