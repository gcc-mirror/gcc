/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */

struct s1 {
    void (*f)(void); /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
    int a;
};

struct s2 {
    union {
	void (*f)(int); /* { dg-error "function pointers not supported with '-mabi=ti' option" } */
	int a;
	long b;
    } u;
};

int test1(struct s1 *p)
{
  return p->a;
  return 1;
}

int test1_unused_arg(struct s1 p, int a)
{
  return a;
}

int test2(struct s2 v)
{
  return 2;
}
