/* { dg-do compile } */
/* { dg-options "-O2 -Wsuggest-attribute=const" } */

extern int extern_const(int a) __attribute__ ((const));

/* Trivial.  */
int
foo1(int a)  /* { dg-bogus "normally" "detect const candidate" } */
{ /* { dg-warning "const" "detect const candidate" { target *-*-* } "8" } */ 
  return extern_const (a);
}

/* Loops known to be normally and extern const calls should be safe.  */

int __attribute__ ((noinline))
foo2(int n)  /* { dg-bogus "normally" "detect const candidate" } */
{ /* { dg-warning "const" "detect const candidate" { target *-*-* } "16" } */
  int ret = 0;
  int i;
  for (i=0; i<n; i++)
    ret+=extern_const (i);
  return ret;
}

/* No warning here; we can work it by ourselves.  */
static int __attribute__ ((noinline))
foo2b(int n)
{
  int ret = 0;
  int i;
  for (i=0; i<n; i++)
    ret+=extern_const (i);
  return ret;
}

/* Unbounded loops are not safe.  */
static int __attribute__ ((noinline))
foo3(int n)  /* { dg-warning "const\[^\n\]* normally" "detect const candidate" } */
{
  int ret = 0;
  int i;
  for (i=0; extern_const (i+n); n++)
    ret+=extern_const (i);
  return ret;
}

int
foo4(int n) /* { dg-warning "const\[^\n\]* normally" "detect const candidate" } */
{
  return foo3(n) + foo2b(n);
} 

int
foo5(int n)  /* { dg-bogus "normally" "detect const candidate" } */
{ /* { dg-warning "const" "detect const candidate" { target *-*-* } "54" } */
  return foo2(n);
} 
