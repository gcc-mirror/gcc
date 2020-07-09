/* { dg-do compile } */
/* { dg-options "-Waddress-of-packed-member" } */

struct r {
  int a[10];
  int b[10][10];
  int ****i4;
};

struct s {
  char c;
  struct r p;
} __attribute__((packed));

struct t {
  char c;
  struct r p __attribute__((packed));
  /* { dg-warning "attribute ignored" "" { target default_packed } .-1 } */
  struct r u;
};

struct s s0;
struct t t0;
int *i0;

void foo (void)
{
  i0 = s0.p.a;               /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = t0.p.a;               /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = s0.p.b[0];            /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = t0.p.b[0];            /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = &s0.p.a[0];           /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = &t0.p.a[0];           /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = &s0.p.b[0][0];        /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = &t0.p.b[0][0];        /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = *s0.p.b;              /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = *t0.p.b;              /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = &**s0.p.b;            /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = &**t0.p.b;            /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = **&s0.p.b;            /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = **&t0.p.b;            /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = &*s0.p.a;             /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = &*t0.p.a;             /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = *&s0.p.a;             /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = *&t0.p.a;             /* { dg-warning "may result in an unaligned pointer value" ""  { target { ! default_packed } } } */
  i0 = t0.u.a;                 /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = t0.u.b[0];              /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = &t0.u.a[0];             /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = &t0.u.b[0][0];          /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = *t0.u.b;                /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = &*t0.u.a;               /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = &**t0.u.b;              /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = ***s0.p.i4;             /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = ***t0.p.i4;             /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = ****&s0.p.i4;           /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = ****&t0.p.i4;           /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = &****s0.p.i4;           /* { dg-bogus "may result in an unaligned pointer value" } */
  i0 = &****t0.p.i4;           /* { dg-bogus "may result in an unaligned pointer value" } */
}
