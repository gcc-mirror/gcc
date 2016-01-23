/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2" } */

int iq;

void
mr(void)
{
  unsigned int i8;

  for (i8 = 0; i8 != 1; i8 += 3) {
    void *f0[] = { f0 };
    int hv;

    for (; hv < 1; ++hv)
      iq = 0;
  }
  ++iq;
}
