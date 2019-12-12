/* { dg-do compile } */
/* { dg-options "-O2" } */

__extension__ typedef __SIZE_TYPE__ size_t;
void *malloc(size_t x);

void foo(void)
{
 volatile int i;
 void *p = malloc(1);
 i;
}
