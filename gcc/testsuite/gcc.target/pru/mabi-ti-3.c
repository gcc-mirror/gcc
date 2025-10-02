/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */


extern void extfunc(void (*fp)(void)); /* { dg-error "function pointers not supported with '-mabi=ti' option" } */

void test(void)
{
  extfunc(test);
}
