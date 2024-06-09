/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f -mabi=lp64d -O2 -mrvv-vector-bits=zvl" } */

extern void abort(void);
extern void exit(int);

void
foo (char *bp, unsigned n)
{
  register char c;
  register char *ep = bp + n;
  register char *sp;

  while (bp < ep)
    {
      sp = bp + 3;
      c = *sp;
      *sp = *bp;
      *bp++ = c;
      sp = bp + 1;
      c = *sp;
      *sp = *bp;
      *bp++ = c;
      bp += 2;
    }
}

int main(void)
{
  int one = 1;

  if (sizeof(int) != 4 * sizeof(char))
    exit(0);

  foo((char *)&one, sizeof(one));
  foo((char *)&one, sizeof(one));

  if (one != 1)
    abort();

  exit(0);
}
