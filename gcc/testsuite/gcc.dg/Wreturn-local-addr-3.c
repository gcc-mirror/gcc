/* PR c/71924 - missing -Wreturn-local-addr returning alloca result
   { dg-do compile }
   { dg-options "-O2 -Wall" }
   { dg-require-effective-target alloca } */

#define ATTR(...) __attribute__ ((__VA_ARGS__))

typedef __INTPTR_TYPE__ intptr_t;

struct A { int a, b, c; };
struct B { int a, b, c[]; };

extern int g1[5], g2[5], g3[5], g4[5], g5[5];

void sink (void*, ...);

/* Verify that a pointer difference expression is handled correctly
   even when converted to a pointer.  */

ATTR (noipa) void*
return_local_diff_cst (void)
{
  int a[5];
  void *p = (void*)(&a[4] - &a[1]);
  return p;
}

ATTR (noipa) void*
return_local_diff_var (int i, int j)
{
  int a[5];
  void *p = (void*)(&a[j] - &a[i]);
  return p;
}

ATTR (noipa) void*
return_2_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  void *p = i < 0 ? a : b;
  return p;         /* { dg-warning "function returns address of local" } */
}

/* Verify that returning the address of a local converted to intptr_t
   is not diagnosed (see bug 90737 for a case the front-end gets wrong).  */

ATTR (noipa) intptr_t
return_int_2_locals (int i)
{
  int a[1];
  int b[2];
  void *p = i < 0 ? a : b;
  return (intptr_t)p;
}

/* Verify that a conditional expression with a pointer first operand
   is handled correctly.  */

ATTR (noipa) void*
return_2_locals_ptrcond (void *q)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  void *p = q ? a : b;
  return p;         /* { dg-warning "function returns address of local" } */
}

/* Verify that a preincrement expression with a pointer operand is
   handled correctly.  */

ATTR (noipa) void*
return_2_locals_ptrinc (void *q)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int *p = q ? a : b;
  return ++p;       /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_3_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */

  void *p = i < 0 ? a : 0 < i ? c : b;
  return p;         /* { dg-warning "function returns address of local" } */
}

/* Verify that a conditional expression with a pointer first operand
   is handled correctly.  */

ATTR (noipa) void*
return_3_locals_ptrcond (void *p, void *q)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */

  void *r = q ? r ? a : b : c;
  return r;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_5_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */
  int d[4];         /* { dg-message "declared here" } */
  int e[5];         /* { dg-message "declared here" } */

  void *p = i < -1 ? a : i < 0 ? b : 1 < i ? e : 0 < i ? d : c;
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_1_global_4_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */
  int d[4];         /* { dg-message "declared here" } */

  void *p = i < -1 ? a : i < 0 ? b : 1 < i ? g1 : 0 < i ? d : c;
  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_2_globals_3_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */

  void *p = i < -1 ? a : i < 0 ? b : 1 < i ? g1 : 0 < i ? g2 : c;
  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_3_globals_2_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */

  void *p = i < -1 ? a : i < 0 ? b : 1 < i ? g1 : 0 < i ? g2 : g3;
  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_4_globals_1_local (int i)
{
  int a[1];         /* { dg-message "declared here" } */

  void *p = i < -1 ? a : i < 0 ? g1 : 1 < i ? g2 : 0 < i ? g4 : g3;
  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_all_globals (int i)
{
  void *p = i < -1 ? g1 : i < 0 ? g2 : 1 < i ? g3 : 0 < i ? g5 : g4;
  return p;
}


ATTR (noipa) void*
return_2_alloca_local_cstoff (int n, int i)
{
  int *a = __builtin_alloca (n);  /* { dg-message "declared here" } */
  int *b = __builtin_alloca (n);  /* { dg-message "declared here" } */
  int *p = i < 0 ? a : b;
  p += 1;
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_alloca_local_cstoff (int n, int i)
{
  int *a = __builtin_alloca (n);  /* { dg-message "declared here" } */
  int b[2];                       /* { dg-message "declared here" } */
  int *p = i < 0 ? a : b;
  p += 1;
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_local_alloca_cstoff (int n, int i)
{
  int a[2];                       /* { dg-message "declared here" } */
  int *b = __builtin_alloca (n);  /* { dg-message "declared here" } */
  int *p = i < 0 ? a : b;
  p += 1;
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_2_locals_cstoff (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int *p = i < 0 ? a : b;
  p += 1;
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_2_globals_3_locals_cstoff (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */

  int *p = i < -1 ? a : i < 0 ? b : 1 < i ? g1 : 0 < i ? g2 : c;
  p += 1;
  sink (p);
  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_3_globals_alloca_local_varoff (int n, int i, int j)
{
  int *a = __builtin_alloca (n);  /* { dg-message "declared here" } */
  int b[2];                       /* { dg-message "declared here" } */

  int *p = i < -1 ? a : i < 0 ? b : 1 < i ? g1 : 0 < i ? g2 : g3;
  p += j;
  sink (p);
  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_3_globals_2_locals_varoff (int i, int j)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */

  int *p = i < -1 ? a : i < 0 ? b : 1 < i ? g1 : 0 < i ? g2 : g3;
  p += j;
  sink (p);
  return p;         /* { dg-warning "function may return address of local" } */
}

