/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2" } */
/* { dg-final { scan-assembler "sltui" } } */

union any {
  int any_i32;
};

extern char *opname;
extern void test1 (int, char *);
extern int iterms;

void
test (union any cv)
{
  int i, on;
  int ix = cv.any_i32;
  for (i = 1; i < iterms; i++)
    {
      on = (ix == 0 || ix == 1) ? 0 : 1;
      if (*opname == '!')
	{
	  on = !on;
	  ++opname;
	}
      test1 (on, opname);
    }
}

