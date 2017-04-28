/* { dg-do compile } */
/* { dg-options "-O0 -Wuninitialized" } */
extern int bar (int);
extern void baz (int *);

int
foo (int i)
{
  int j; /* { dg-warning "'j' may be used uninitialized in this function" "uninitialized" { xfail *-*-* } } */

  if (bar (i)) {
    baz (&j);
  } else {
  }

  return j;
}

int foo2( void ) {
  int rc;
  return rc; /* { dg-warning "'rc' is used uninitialized in this function" } */
  *&rc = 0;
}

extern int printf(const char *, ...);
void frob(int *pi);

int main(void)
{
  int i; 
  printf("i = %d\n", i); /* { dg-warning "'i' is used uninitialized in this function" } */
  frob(&i);

  return 0;
}

void foo3(int*);
void bar3(void) { 
  int x; 
  if(x) /* { dg-warning "'x' is used uninitialized in this function" } */
    foo3(&x); 
}
