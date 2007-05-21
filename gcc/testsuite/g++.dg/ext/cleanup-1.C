/* { dg-do compile } */
/* { dg-options "-Wall" } */
/* Validate expected warnings and errors.  */

#define U	__attribute__((unused))
#define C(x)	__attribute__((cleanup(x)))

static int f1(void *x U) { return 0; }
static void f2() { } /* { dg-error "too many arguments" } */
static void f3(void) { } /* { dg-error "too many arguments" } */
static void f4(void *x U) { }
static void f5(int *x U) { }
static void f6(double *x U) { }
static void f7(const int *x U) { }
static void f8(const int *x U, int y U) { } /* { dg-error "too few arguments" } */
static void f9(int x U) { }

void test(void)
{
  int o1 C(f1);
  int o2 C(f2);         /* { dg-error "at this point" } */
  int o3 C(f3);		/* { dg-error "at this point" } */
  int o4 C(f4);
  int o5 C(f5);
  int o6 C(f6);		/* { dg-error "cannot convert" } */
  int o7 C(f7);
  int o8 C(f8);		/* { dg-error "at this point" } */
  int o9 C(f9);		/* { dg-error "conversion" } */
  int o10 U C(undef);	/* { dg-error "not a function" } */
  int o11 U C(o1);	/* { dg-error "not a function" } */
  int o12 U C("f1");	/* { dg-error "not an identifier" } */
  static int o13 U C(f1); /* { dg-warning "attribute ignored" } */
}

int o14 C(f1);		/* { dg-warning "attribute ignored" } */
void t15(int o U C(f1)) {} /* { dg-warning "attribute ignored" } */
