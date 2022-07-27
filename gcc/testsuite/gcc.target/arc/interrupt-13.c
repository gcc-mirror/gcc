/* { dg-options "-O2" } */

extern int foo (int *);

void __attribute__((interrupt("ilink")))
irq (void)
{
  struct {
    int x0;
    int x1;
  } a = {1 ,2};
  foo ((int *)&a);
}

/* { dg-final { scan-assembler "add_s\\s+sp,sp,8.*pop_s\\s+r0" } } */
