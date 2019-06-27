/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */


extern void extfunc(void (*fp)(void));

void test(void)
{
  extfunc(test); /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
}
