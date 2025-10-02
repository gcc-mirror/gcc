/* Test TI ABI unsupported constructs */

/* { dg-do assemble } */
/* { dg-options "-O1 -mabi=ti" } */

struct s0 {
    int f0 : 2; /* { dg-error "bit-fields not supported with '-mabi=ti' option" } */
    int f1;
};
struct s0 g_s;

struct s1 {
    int f : 2; /* { dg-error "bit-fields not supported with '-mabi=ti' option" } */
    int a;
};
struct s1 g_s1;
int test1(void)
{
  struct s1 s;

  return sizeof(s);
}

struct s2 {
    int a2;
    int f2 : 3; /* { dg-error "bit-fields not supported with '-mabi=ti' option" } */
};
struct s22 {
	struct s2 *p;
};
int test2(struct s22 *s)
{
  return s->p->f2 + s->p->a2;
}

int test2a(struct s2 *s)
{
  return s->f2 - s->a2;
}

struct s3 {
    int a3;
    int f3 : 3; /* { dg-error "bit-fields not supported with '-mabi=ti' option" } */
};
int test3(struct s3 s)
{
  return 0;
}
