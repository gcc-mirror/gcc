extern void exit(int);
extern void abort (void);

extern void * memcpy (void *, const void *, unsigned int);
int foo (void *, void *, unsigned int c);

int src[10];
int dst[10];

int main()
{
   if (foo (dst, src, 10) != 0)
     abort();
   exit(0);
}

int foo (void *a, void *b, unsigned int c)
{
  if (c == 0)
    return 1;

  memcpy (a, b, c);
  return 0;
}
