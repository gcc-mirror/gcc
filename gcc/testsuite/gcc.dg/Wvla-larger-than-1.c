/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Wvla-larger-than=100 -O2" } */

typedef __SIZE_TYPE__ size_t;

extern void useit (char *);

int num;

void test_vlas (size_t num)
{
  char str2[num];		/* { dg-warning "unbounded use" } */
  useit(str2);

  num = 98;
  for (int i=0; i < 1234; ++i) {
    char str[num];	        // OK, VLA in a loop, but it is a
				// known size *AND* the compiler takes
				// care of cleaning up between
				// iterations with
				// __builtin_stack_restore.
    useit(str);
  }
}
