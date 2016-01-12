// PR c++/69213
// { dg-do compile }
// { dg-options "-O3" }

int a, b;

void
foo (void)
{
  __asm__ ("# %0" : : : "memory");	// { dg-error "operand number out of range" }
}

int
main ()
{
  for (; a < 0; b++)
    a = b;
}
