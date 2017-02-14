/* { dg-do compile } */
/* { dg-options "-O2" } */
void *malloc(unsigned long x);

void foo(void)
{
 volatile int i;
 malloc(1);
 i;
}
