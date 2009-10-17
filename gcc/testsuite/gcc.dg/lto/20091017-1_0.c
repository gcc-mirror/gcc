/* { dg-lto-do run } */

struct X {
    int i;
};
extern void foo (void *);
extern void abort (void);
int main ()
{
  struct X *p;
  foo(&p);
  if (p != (struct X *)0)
    abort ();
  return 0;
}
