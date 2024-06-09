/* { dg-lto-do run } */
/* { dg-lto-options {{-O3 -flto}} }  */

struct bar {int a;};
struct foo {int a;};
struct barp {struct bar *f; struct bar *g;};
extern struct foo **ptr;
int test2 (void *);
int test3 (void *);
int
testb(void)
{
  struct bar *fp;
  test2 ((void *)&fp);
  fp = (void *) 0;
  (*ptr)++;
  test3 ((void *)&fp);
}
