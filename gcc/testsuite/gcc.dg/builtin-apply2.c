/* PR target/12503 */
/* Origin: <pierre.nguyen-tuong@asim.lip6.fr> */

/* Verify that __builtin_apply behaves correctly on targets
   with pre-pushed arguments (e.g. SPARC).  */

/* { dg-do run } */
   

#define INTEGER_ARG  5

typedef __SIZE_TYPE__ size_t;

extern void abort(void);

void foo(char *name, double d, double e, double f, int g)
{
  if (g != INTEGER_ARG)
    abort();
}

void bar(char *name, ...)
{
  size_t size;
  void *arguments;

  size = sizeof(char *) + 3 * sizeof(double) + sizeof(int);

  arguments = __builtin_apply_args();

  __builtin_apply(foo, arguments, size);
}

int main(void)
{
  bar("eeee", 5.444567, 8.90765, 4.567789, INTEGER_ARG);

  return 0;
}
