/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

int a;
void* p;

void foo (void)
{
  switch (a)
  {
    a0: case 0:   p = &&a1;
    a1: case 1:   p = &&a2;
    a2: default:  p = &&a1;
  }
  goto *p;
}
