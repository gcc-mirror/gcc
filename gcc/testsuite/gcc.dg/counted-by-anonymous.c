/* Testing the correct usage of attribute counted_by for anonymous
   structures.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#define __counted_by(member) \
    __attribute__((__counted_by__(member)))

struct fam_in_union {
  int count;
  union {
    char a;
    char fam[] __counted_by(count);
  };
};

struct fam_in_union_2 {
  int count;
  union inside {
    char a;
    char fam[] __counted_by(count); /* { dg-error "attribute is not a field declaration in the same structure as" } */
  } inside_u;
};

struct fam_in_struct {
  int count;
  struct {
    char a;
    char fam[] __counted_by(count);
  };
};

struct fam_in_struct_2 {
  int count;
  struct insidestruct {
    char a;
    char fam[] __counted_by(count); /* { dg-error "attribute is not a field declaration in the same structure as" } */
  } inside_s;
};

struct pointer_in_union {
  union {
    char a;
    char* p __counted_by(count);
  };
  int count;
};

struct pointer_in_union_2 {
  union insideunion {
    char a;
    char* p __counted_by(count); /* { dg-error "attribute is not a field declaration in the same structure as" } */
  } inside_u;
  int count;
};

struct pointer_in_union_3 {
  union {
    char b;
    char* p __counted_by(countp); /* { dg-error "attribute is not a field declaration with an integer type" } */

  };
  float countp;
};

struct pointer_in_struct {
  struct {
    int count_q;
    char *p __counted_by(count_p);
    float *q __counted_by(count_q);
    int count_fam;
    struct {
      int count_p;
      char a;
      char fam[] __counted_by(count_fam);
    };
  };
};

struct nested_mixed {
  struct {
    union {
      int b;
      float f;
    };
    int n;
  };
  struct {
    int *pointer __counted_by(n);
    float *pointer_2 __counted_by(f); /* { dg-error "attribute is not a field declaration with an integer type" } */
    char c[] __counted_by(b);
  };
} *array_nested_annotated;

/* Support an untagged type as its own top-level type.  */

/* A. Variable declaration.  */

struct { int a0; char b0[] __attribute__ ((counted_by (a0))); } x0;
struct { char b0[] __attribute__ ((counted_by (a0))); } x00; /* { dg-error "attribute is not a field declaration in the same structure as" } */

struct { int a; char b[] __attribute__ ((counted_by (a))); } *x;
struct { char b1[] __attribute__ ((counted_by (a1))); } *x1; /* { dg-error "attribute is not a field declaration in the same structure as" } */

struct { char *e __attribute__ ((counted_by (f))); int f; } *x2;
struct { char *e1 __attribute__ ((counted_by (f1))); } *x3; /* { dg-error "attribute is not a field declaration in the same structure as" } */

struct { char *e4 __attribute__ ((counted_by (f4))); int f4; } **x4;
struct { char *e5 __attribute__ ((counted_by (f5))); } **x5; /* { dg-error "attribute is not a field declaration in the same structure as" } */

/* B. Function declaration and definitions.  */
struct { int c0; char d0[] __attribute__ ((counted_by (c0))); } func0 (int a, int b);
struct { char d[] __attribute__ ((counted_by (c))); } func00 (int a, int b); /* { dg-error "attribute is not a field declaration in the same structure as" } */

struct { int c; char d[] __attribute__ ((counted_by (c))); } *func (int a, int b);
struct { char d1[] __attribute__ ((counted_by (c1))); } *func1 (int a, int b); /* { dg-error "attribute is not a field declaration in the same structure as" } */

struct { int c2; char *d2 __attribute__ ((counted_by (c2))); } *func2 () { return 0; }
struct { char *d3 __attribute__ ((counted_by (c3))); } *func3 () { return 0; } /* { dg-error "attribute is not a field declaration in the same structure as" } */

struct { int c4; char *d4 __attribute__ ((counted_by (c4))); } **func4 () { return 0; }
struct { char *d5 __attribute__ ((counted_by (c5))); } **func5 () { return 0; } /* { dg-error "attribute is not a field declaration in the same structure as" } */

/* C. Parameter declaration.  */
void func6 (struct { float *f __attribute__ ((counted_by (g))); int g; } *pa, int count); /* { dg-warning "anonymous struct declared inside parameter list will not be visible outside" } */
void func7 (struct { float *f1 __attribute__ ((counted_by (g1)));} *pa, int count); /* { dg-error "attribute is not a field declaration in the same structure as" } */
										    /* { dg-warning "anonymous struct declared inside parameter list will not be visible outside" "" { target *-*-* } .-1 } */

/* D. Typename.  */
int foo ()
{
  int res = sizeof (struct {int count; char *p __attribute__ ((counted_by (count))); });

  res += alignof (struct {char *p1 __attribute__ ((counted_by (count1))); }); /* { dg-error "attribute is not a field declaration in the same structure as" } */
  return res;
}

typedef struct {
 int mc;
 char *d __attribute__ ((counted_by (mc)));
} mys;

typedef struct {
 char *md1 __attribute__ ((counted_by (mc1))); /* { dg-error "attribute is not a field declaration in the same structure as" } */
} mys1;


/* Support an unnamed field with a named struct/union.  */
struct s0 { struct { int a0; char b0[] __attribute__ ((counted_by (a0))); } x; } yy0;
struct s00 { struct { char c0[] __attribute__ ((counted_by (d0))); } x; } yy00; /* { dg-error "attribute is not a field declaration in the same structure as" } */

struct s { struct { int a; char b[] __attribute__ ((counted_by (a))); } *x; } *y;
struct s1 { struct { char c[] __attribute__ ((counted_by (d))); } *x; } *yy; /* { dg-error "attribute is not a field declaration in the same structure as" } */

struct s2 { struct { char *b __attribute__ ((counted_by (a))); int a; } *x; } *y2;
struct s3 { struct { char *c __attribute__ ((counted_by (d))); } *x; } *y3; /* { dg-error "attribute is not a field declaration in the same structure as" } */

struct s4 { struct { char *b4 __attribute__ ((counted_by (a4))); int a4; } x[3]; } *y4;
struct s5 { struct { char *c5 __attribute__ ((counted_by (d5))); } x[4]; } *y5; /* { dg-error "attribute is not a field declaration in the same structure as" } */

struct s6 { struct { char *b6 __attribute__ ((counted_by (a6))); int a6; } *x[3]; } *y6;

struct s7 { struct { char *c7 __attribute__ ((counted_by (d7))); } *x[4]; } *y7; /* { dg-error "attribute is not a field declaration in the same structure as" } */
