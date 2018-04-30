/* { dg-add-options stack_size } */

void
bcopy1 (s, d, c)
     long long *s;
     long long *d;
     int c;
{
  int i;
  c = c / 8;
  for (i = 0;  i < c;  i++)
    d[i] = s[i];
}

void
bcopy2 (s, d, c)
     long *s;
     long *d;
     int c;
{
  int i;
  c = c / 4;
  for (i = 0;  i < c;  i++)
    d[i] = s[i];
}


void
bcopy3 (s, d, c)
     char *s;
     char *d;
     int c;
{
  long long z0, z1;
  int r = d - s;

  int i;

  c /= 16;

  z0 = *((long long *) s);
  s += 8;
  z1 = *((long long *) s);
  s += 8;
  for (i = 0; i < c; i++)
    {
      *(long long *)(s + r) = z0;
      z0 = *((long long *) s);
      s += 8;
      *(long long *)(s + r) = z1;
      z1 = *((long long *) s);
      s += 8;
    }
}

#if defined(STACK_SIZE) && STACK_SIZE < 16384
#define BYTES STACK_SIZE
#else
#define BYTES 16384
#endif

main ()
{
  long long s[BYTES / 8];
  long long d[BYTES / 8];
  int i;

  for (i = 1; i < 67108864 / BYTES; i++)
    bcopy (s, d, BYTES);
}
