/* Origin: PR target/6981 from Mattias Engdegaard <mattias@virtutech.se>.  */
/* { dg-require-effective-target int32plus } */

void exit (int);
void abort (void);

unsigned long long *cp, m;

void foo (void)
{
}

void bar (unsigned rop, unsigned long long *r)
{
  unsigned rs1, rs2, rd;

top:
  rs2 = (rop >> 23) & 0x1ff;
  rs1 = (rop >> 9) & 0x1ff;
  rd = rop & 0x1ff;

  *cp = 1;
  m = r[rs1] + r[rs2];
  *cp = 2;
  foo();
  if (!rd)
    goto top;
  r[rd] = 1;
}

int main(void)
{
  static unsigned long long r[64];
  unsigned long long cr;
  cp = &cr;

  r[4] = 47;
  r[8] = 11;
  bar((8 << 23) | (4 << 9) | 15, r);

  if (m != 47 + 11)
    abort ();
  exit (0);
}
