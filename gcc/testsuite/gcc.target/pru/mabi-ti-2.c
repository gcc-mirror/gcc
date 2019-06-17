/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */


struct big {
	char c[9];
};

struct big test(void)
{ /* { dg-error "large return values not supported with '-mabi=ti' option" } */
  static struct big b;
  return b;
}
