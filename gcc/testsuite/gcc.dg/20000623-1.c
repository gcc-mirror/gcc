/* { dg-do run } */
/* { dg-options "-O3 -fno-strict-aliasing" } */

extern void exit (int);
extern void abort (void);

struct foos { int l; }; 
int foo;
static struct foos *getfoo(void);
int main (void)
{
  struct foos *f = getfoo();
  f->l = 1;
  foo = 2;
  if (f->l == 1)
    abort();
  exit(0);
}
static struct foos *getfoo(void) 
{ return (struct foos *)&foo; }
