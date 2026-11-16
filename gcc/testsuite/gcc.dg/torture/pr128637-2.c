/* { dg-do run { target { lp64 && le } } } */

void abort (void);
void *malloc(unsigned long);
void *memset(void *, int, unsigned long);
void *memcpy(void *, const void *, unsigned long);

int x;
long r;

int main(void)
{
  unsigned char *b = malloc(sizeof (long long));
  memset(b, 0, 8);
  *(long long *)b = 1;
  b[4] = 1;
  if (x)
    r = *(long *)b;
  long long t;
  memcpy(&t, b, 8);
  if (t != 0x100000001)
    abort ();
  return 0;
}
