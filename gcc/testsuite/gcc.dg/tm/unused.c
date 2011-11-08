/* { dg-do compile } */
/* { dg-options "-fgnu-tm -Wall" } */

__attribute__((transaction_safe))
static int unused_func ()	/* { dg-warning "defined but not used" } */
{
  return 12345;
}

int main()
{
  return 0;
}

/* { dg-final { scan-assembler "_ZGTt11unused_func:" } } */
