/* Regression test for PR/77822.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=zEC12" } */

void m_fn1();

char m_datawidth;
char m_subunits;
int m_subunit_infos[1];

int a;
long b;
void m_fn1() {
  int c = 32, d = m_datawidth / c;
  for (int e = 0; e < d; e++) {
    int f = e * 32;
    if (b >> f & 1)
      m_subunit_infos[m_subunits] = a;
  }
}

/* { dg-final { scan-assembler-not "risbg.*-\[0-9\]+\\\+1\n" } } */
