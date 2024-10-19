/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */

struct s1 {
    int (*f)(void);  /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
    int a;
};

struct s2 {
    int (*f)(short);  /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
    int a;
};

struct s3 {
    int (*f)(char);  /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
    int a;
    int b;
    int c;
};

extern struct s1 g_s1;
extern struct s2 g_s2;
struct s3 g_s3;

int test1(void)
{
  return g_s1.f();
}

int test2(void)
{
  return g_s2.a;
}
