/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */

struct s1 {
    int (*f)(void);
    int a;
};

extern struct s1 s;

int test1(void)
{
  return s.f(); /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
}

int test2(void)
{
  return s.a; /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
}
