/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

static t1 *a;           /* { dg-error "unknown type name 't1'" } */

int z;                  /* { dg-message "previous declaration of 'z'" } */
typedef t2 *z;          /* { dg-error "unknown type name 't2'" } */
/* { dg-error "'z' redeclared " "redeclared" { target *-*-* } .-1 } */

extern t3 p1(void);     /* { dg-error "unknown type name 't3'" } */
int p2(const t4 x);     /* { dg-error "unknown type name 't4'" } */
int p3(const t1 x);     /* { dg-error "unknown type name 't1'" } */ /* dup??? */
int p4(t5 (*x)(void));  /* { dg-error "unknown type name 't5'" } */
int p5(t6 *);           /* { dg-error "unknown type name 't6'" } */
int p6(t7 x);           /* { dg-error "unknown type name 't7'" } */
int p7(t8[]);           /* { dg-error "unknown type name 't8'" } */
int p8(int, t9);        /* { dg-error "unknown type name 't9'" } */

struct s {
  const t1 a;           /* { dg-error "unknown type name 't1'" } */ /* dup??? */
  const t10 b;          /* { dg-error "unknown type name 't10'" } */
  int b;                /* { dg-error "duplicate member" } */
};

typeof (z) c1;

typeof (x1) c2;
/* { dg-error "undeclared" "undeclared" { target *-*-* } .-1 } */
/* { dg-bogus "unknown type name 'x1'" "unknown" { target *-*-* } .-2 } */

typeof (const t11) c3;  /* { dg-error "unknown type name 't11'" } */

typeof (t12 *) c3;
/* { dg-error "unknown type name 't12'" "t12" { xfail *-*-* } .-1 } */
/* { dg-bogus "undeclared" "undeclared" { xfail *-*-* } .-2 } */
/* { dg-bogus "expected expression before" "expected" { xfail *-*-* } .-3 } */

int recover1;

int s0 = sizeof (z);

int s1 = sizeof (x2);
/* { dg-error "undeclared" "undeclared" { target *-*-* } .-1 } */
/* { dg-bogus "unknown type name 'x2'" "unknown" { target *-*-* } .-2 } */

int s2 = sizeof (const t13);   /* { dg-error "unknown type name 't13'" } */

int s3 = sizeof (t14 *);
/* { dg-error "unknown type name 't14'" "t14" { xfail *-*-* } .-1 } */
/* { dg-bogus "undeclared" "undeclared" { xfail *-*-* } .-2 } */
/* { dg-bogus "expected expression before" "expected" { xfail *-*-* } .-3 } */

int recover2;

int a0 = __alignof__ (z);

int a1 = __alignof__ (x3);          /* { dg-error "undeclared" } */
/* { dg-bogus "unknown type name 'x3'" "" { target *-*-* } .-1 } */

int a2 = __alignof__ (const t15);   /* { dg-error "unknown type name 't15'" } */

int a3 = __alignof__ (t16 *);
/* { dg-error "unknown type name 't16'" "t16" { xfail *-*-* } .-1 } */
/* { dg-bogus "undeclared" "undeclared" { xfail *-*-* } .-2 } */
/* { dg-bogus "expected expression before" "expected" { xfail *-*-* } .-3 } */

int recover3;


/* Cannot detect (undefd_type *) or (undefd_type (*) because it would
   require 3 tokens of lookahead (same as above).  */

const char *f1()
{
  return (const t17) "abc";       /* { dg-error "unknown type name 't17'" "t17" } */
/* { dg-bogus "expected" "expected" { target *-*-* } .-1 } */
}

const char *f2()
{
  return (const t18 *) "abc";     /* { dg-error "unknown type name 't18'" "t18" } */
/* { dg-bogus "expected" "expected" { target *-*-* } .-1 } */
}


/* The parser has problems distinguishing semantic and syntactic errors,
   so it emits a wrong "expected ')'" error here.  */

void *f3(int x)
{
  return (void *) ((void *(*)(t19)) f3);       /* { dg-error "unknown type name 't19'" "t19" } */
/* { dg-bogus "expected" "expected" { xfail *-*-* } .-1 } */
}

const void *f4()
{
  return &((const t20){1});       /* { dg-error "unknown type name 't20'" } */
/* { dg-bogus "return discards 'const'" "discards" { target *-*-* } .-1 } */
/* { dg-bogus "expected" "expected" { target *-*-* } .-2 } */
}

int f5(__builtin_va_list ap)
{
  int x = __builtin_va_arg (ap, t21);       /* { dg-error "unknown type name 't21'" } */
  int y = __builtin_va_arg (ap, const t22); /* { dg-error "unknown type name 't22'" } */
}

int f6(void)
{
  return __builtin_offsetof (t23, field); /* { dg-error "unknown type name 't23'" "t23" } */
/* { dg-bogus "request for member" "request" { target *-*-* } .-1 } */
}
