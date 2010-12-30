/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

static t1 *a;           /* { dg-error "unknown type name 't1'" } */

int z;                  /* { dg-message "previous declaration of 'z'" } */
typedef t2 *z;          /* { dg-error "unknown type name 't2'" } */
/* { dg-error "'z' redeclared " "" { target *-*-* } 7 } */

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
typeof (x1) c2;         /* { dg-error "undeclared" } */
typeof (const t11) c3;  /* { dg-error "unknown type name 't11'" } */
typeof (t12 *) c3;      /* { dg-error "unknown type name 't12'" "" { xfail *-*-* } } */
/* { dg-bogus "unknown type name 'x1'" "" { target *-*-* } 26 } */
/* { dg-bogus "undeclared" "" { xfail *-*-* } 28 } */
/* { dg-bogus "expected expression before" "" { xfail *-*-* } 28 } */

int recover1;

int s0 = sizeof (z);
int s1 = sizeof (x2);          /* { dg-error "undeclared" } */
int s2 = sizeof (const t13);   /* { dg-error "unknown type name 't13'" } */
int s3 = sizeof (t14 *);       /* { dg-error "unknown type name 't14'" "" { xfail *-*-* } } */

int recover2;

/* { dg-bogus "unknown type name 'x2'" "" { target *-*-* } 36 } */
/* { dg-bogus "undeclared" "" { xfail *-*-* } 38 } */
/* { dg-bogus "expected expression before" "" { xfail *-*-* } 38 } */

int a0 = __alignof__ (z);
int a1 = __alignof__ (x3);          /* { dg-error "undeclared" } */
int a2 = __alignof__ (const t15);   /* { dg-error "unknown type name 't15'" } */
int a3 = __alignof__ (t16 *);       /* { dg-error "unknown type name 't16'" "" { xfail *-*-* } } */

int recover3;

/* { dg-bogus "unknown type name 'x3'" "" { target *-*-* } 47 } */
/* { dg-bogus "undeclared" "" { xfail *-*-* } 49 } */
/* { dg-bogus "expected expression before" "" { xfail *-*-* } 49 } */


/* Cannot detect (undefd_type *) or (undefd_type (*) because it would
   require 3 tokens of lookahead (same as above).  */

const char *f1()
{
  return (const t17) "abc";       /* { dg-error "unknown type name 't17'" } */
/* { dg-bogus "expected" "" { target *-*-* } 63 } */
}

const char *f2()
{
  return (const t18 *) "abc";     /* { dg-error "unknown type name 't18'" } */
/* { dg-bogus "expected" "" { target *-*-* } 69 } */
}


/* The parser has problems distinguishing semantic and syntactic errors,
   so it emits a wrong "expected ')'" error here.  */

void *f3(int x)
{
  return (void *) ((void *(*)(t19)) f3);       /* { dg-error "unknown type name 't19'" } */
/* { dg-bogus "expected" "" { xfail *-*-* } 79 } */
}

const void *f4()
{
  return &((const t20){1});       /* { dg-error "unknown type name 't20'" } */
/* { dg-bogus "return discards 'const'" "" { target *-*-* } 85 } */
/* { dg-bogus "expected" "" { target *-*-* } 85 } */
}

int f5(__builtin_va_list ap)
{
  int x = __builtin_va_arg (ap, t21);       /* { dg-error "unknown type name 't21'" } */
  int y = __builtin_va_arg (ap, const t22); /* { dg-error "unknown type name 't22'" } */
}

int f6(void)
{
  return __builtin_offsetof (t23, field); /* { dg-error "unknown type name 't23'" } */
/* { dg-bogus "request for member" "" { target *-*-* } 98 } */
}
