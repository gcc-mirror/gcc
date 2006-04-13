/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */

struct x {
  int selector;
};

void f()
{
  struct x* addr;
  __asm__("mov %%gs,%0":"=r" ((unsigned short)addr->selector)); /* { dg-error "lvalue" "casts make rvalues" } */
}
