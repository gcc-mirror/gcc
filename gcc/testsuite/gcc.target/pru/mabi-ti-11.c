/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */

struct s1 {
    int (*f)(void);  /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
    int a;
};

int test1(void)
{
  return ((struct s1 *)0x11223344)->a;
}
