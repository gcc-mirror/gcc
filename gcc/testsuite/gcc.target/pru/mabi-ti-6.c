/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */


extern void (*extfuncp)(int);

void test(void)
{
  extfuncp(1); /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
}
