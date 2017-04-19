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

enum e3
{
  F = sizeof (struct t3),	/* { dg-bogus "invalid in C\[+\]\[+\]" } */
  /* { dg-error "invalid application of 'sizeof'" "" { target *-*-* } .-1 } */
  G = __alignof__ (struct t4), /* { dg-bogus "invalid in C\[+\]\[+\]" } */
  /* { dg-error "invalid application of '__alignof__'" "" { target *-*-* } .-1 } */
  H
};

__typeof__ (struct s5 { int i; }) v5; /* { dg-warning "invalid in C\[+\]\[+\]" } */
__typeof__ (struct t5) w5; /* { dg-bogus "invalid in C\[+\]\[+\]" } */
  /* { dg-error "storage size of 'w5' isn't known" "" { target *-*-* } .-1 } */

int
f1 (struct s1 *p)
{
  return ((struct s6 { int j; } *) p)->j;  /* { dg-warning "invalid in C\[+\]\[+\]" } */
}

void *
f2 (struct s1 *p)
{
  return ((struct t6 *) p);  /* { dg-bogus "invalid in C\[+\]\[+\]" } */
}

int
f3 (struct s1 *p)
{
  return (__extension__ (struct s7 { int j; } *)p)->j;
}

int
f4 ()
{
  return (struct s8 { int i; }) { 0 }.i;  /* { dg-warning "invalid in C\[+\]\[+\]" } */
}

void *
f5 ()
{
  return &((struct t8) { });  /* { dg-warning "invalid in C\[+\]\[+\]" } */
}

/* { dg-error "invalid use of undefined type" "" { target *-*-* } 65 } */
