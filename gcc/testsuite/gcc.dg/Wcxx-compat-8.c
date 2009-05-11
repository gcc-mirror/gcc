/* { dg-do compile } */
/* { dg-options "-Wc++-compat" } */

struct s1
{
  enum e1	/* { dg-message "note: enum type defined here" } */
  {
    A = sizeof (struct s2 { int i; }),	/* { dg-warning "invalid in C\[+\]\[+\]" } */
    B
  } f1;
};
struct s2 v1;	/* Don't issue another warning about s2.  */
enum e1 v2;	/* { dg-warning "not visible in C\[+\]\[+\]" } */

enum e2
{
  C = sizeof (struct s3 { int i; }),	/* { dg-warning "invalid in C\[+\]\[+\]" } */
  D = __alignof__ (struct s4 { int i; }), /* { dg-warning "invalid in C\[+\]\[+\]" } */
  E
};

struct s3 v3;
int v4 = C;

__typeof__ (struct s5 { int i; }) v5; /* { dg-warning "invalid in C\[+\]\[+\]" } */

int
f1 (struct s1 *p)
{
  return ((struct s6 { int j; } *) p)->j;  /* { dg-warning "invalid in C\[+\]\[+\]" } */
}

int
f2 (struct s1 *p)
{
  return (__extension__ (struct s7 { int j; } *)p)->j;
}

int
f3 ()
{
  return (struct s8 { int i; }) { 0 }.i;  /* { dg-warning "invalid in C\[+\]\[+\]" } */
}
