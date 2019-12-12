/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */


int test(int a, int b, void (*fp)(void))
{ /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
  return a+b;
}
