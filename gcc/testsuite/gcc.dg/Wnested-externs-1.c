/* Test -Wnested-externs.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wnested-externs" } */

int a;
static int b;
extern int c;
void f0(void);
static void f1(void);

void
g(void)
{
  extern int a; /* { dg-warning "nested extern declaration of 'a'" } */
  extern int b; /* { dg-warning "nested extern declaration of 'b'" } */
  extern int c; /* { dg-warning "nested extern declaration of 'c'" } */
  extern int d; /* { dg-warning "nested extern declaration of 'd'" } */
  extern void f0(void); /* { dg-warning "nested extern declaration of 'f0'" } */
  extern void f1(void); /* { dg-warning "nested extern declaration of 'f1'" } */
  extern void f2(void); /* { dg-warning "nested extern declaration of 'f2'" } */
}
