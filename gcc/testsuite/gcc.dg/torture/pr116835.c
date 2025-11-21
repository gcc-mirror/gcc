/* { dg-do run { target { weak_undefined } } } */
/* { dg-add-options weak_undefined } */

/* PR tree-optimization/116835 */
/* phiprop would prop into the loop the load of b
   and also prop the load of a before the loop.
   Which is incorrect as a is a weak symbol.  */

struct s1
{
  int t;
  int t1;
};
typedef struct s1 type;
extern type a __attribute__((weak));
int t;
type b;
type bar (int c) __attribute__((noipa, noinline));
type 
bar (int c)
{
  t = 1;
  type *p = &a;
  for (int j = 0; j < c; ++j)
    p = &b;
  return *p;
}

int main(void)
{
 if (bar(&a == nullptr).t)
   __builtin_abort();
}
