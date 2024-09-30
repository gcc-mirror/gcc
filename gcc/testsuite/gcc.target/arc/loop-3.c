/* { dg-do assemble } */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-sdata" } */

/* This example will fail to assemble if the last instruction is a
   branch with delay slot.  */
int d;
extern char * fn2 (void);

void fn1(void)
{
  char *a = fn2();
  for (;;) {
    long long b;
    int e = 8;
    for (; e <= 63; e += 7) {
      long c = *a++;
      b += c & e;
      if (c & 28)
        break;
    }
    d = b;
  }
}

/* { dg-final { scan-assembler "bne.*\\.L2" } } */
/* { dg-final { scan-assembler-not "add.eq" } } */
