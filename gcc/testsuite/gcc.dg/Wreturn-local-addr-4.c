/* PR c/71924 - missing -Wreturn-local-addr returning alloca result
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#define ATTR(...) __attribute__ ((__VA_ARGS__))

struct A { int a, b, c; };
struct B { int a, b, c[]; };

extern int g1[5], g2[5], g3[5], g4[5], g5[5];

void sink (void*, ...);

ATTR (noipa) void*
return_2_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  void *p = b;
  if (i < 0)
    p = a;

  sink (p);

  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_2_locals_after_2_globals (int i, int j)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */

  int *p;
  if (i < 0)
    p = g1;
  else
    p = g2;

  sink (p);

  if (j < 0)
    p = a;
  else
    p = b;

  sink (p);

  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_3_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */

  void *p = b + 1;
  if (i < 0)
    p = a;
  else if (0 < i)
    p = c + 2;

  sink (p);

  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_5_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */
  int d[4];         /* { dg-message "declared here" } */
  int e[5];         /* { dg-message "declared here" } */

  void *p = &c[2];
  if (i < -1)
    p = a;
  else if (i < 0)
    p = &b[1];
  else if (1 < i)
    p = &e[4];
  else if (0 < i)
    p = &d[3];

  sink (p);

  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_5_locals_switch (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */
  int d[4];         /* { dg-message "declared here" } */
  int e[5];         /* { dg-message "declared here" } */

  void *p = 0;

  switch (i)
    {
    case 0: p = &a[1]; break;
    case 1: p = &b[2]; break;
    case 2: p = &c[3]; break;
    case 3: p = &d[4]; break;
    default: p = &e[5]; break;
    }

  sink (p);

  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_1_global_4_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */
  int d[4];         /* { dg-message "declared here" } */

  void *p = c;
  if (i < -1)
    sink (p = a);
  else if (i < 0)
    sink (p = b);
  else if (1 < i)
    sink (p = g1);
  else if (0 < i)
    sink (p = d);

  sink (p, a, b, c, d);

  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_1_global_4_locals_switch (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */
  int d[4];         /* { dg-message "declared here" } */

  void *p = 0;

  switch (i)
    {
    case 0: p = &a[0]; break;
    case 1: p = &b[1]; break;
    case 2: p = &c[2]; break;
    case 3: p = &d[3]; break;
    }

  sink (p);

  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_2_globals_3_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */
  int c[3];         /* { dg-message "declared here" } */

  void *p = c;
  if (i < -1)
    p = a;
  else if (i < 0)
    p = b;
  else if (1 < i)
    p = g1;
  else if (0 < i)
    p = g2;

  sink (p);

  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_3_globals_2_locals (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */

  void *p = g3;
  if (i < -1)
    p = a;
  else if (i < 0)
    p = b;
  else if (1 < i)
    p = g1;
  else if (0 < i)
    p = g2;

  sink (p);

  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_4_globals_1_local (int i)
{
  int a[1];         /* { dg-message "declared here" } */

  void *p = g3;
  if (i < -1)
    p = a;
  else if (i < 0)
    p = g1;
  else if (1 < i)
    p = g2;
  else if (0 < i)
    p = g4;

  sink (p);

  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_all_globals (int i)
{
  void *p = g4;
  if (i < -1)
    p = g1;
  else if (i < 0)
    p = g2;
  else if (1 < i)
    p = g3;
  else if (0 < i)
    p = g5;
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

  int *p = b;
  if (i < 0)
    p = a;

  p += 1;
  sink (p);

  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_local_alloca_cstoff (int n, int i)
{
  int a[2];                       /* { dg-message "declared here" } */
  int *b = __builtin_alloca (n);  /* { dg-message "declared here" } */
  int *p = b;
  if (i < 0)
    p = a;

  p += 1;
  sink (p);

  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_2_locals_cstoff (int i)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */

  int *p = b;
  if (i < 0)
    p = a;

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

  int *p = c;
  if (i < -1)
    p = a;
  else if (i < 0)
    p = b;
  else if (1 < i)
    p = g1;
  else if (0 < i)
    p = g2;

  p += 1;
  sink (p);

  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_3_globals_alloca_local_varoff (int n, int i, int j)
{
  int *a = __builtin_alloca (n);  /* { dg-message "declared here" } */
  int b[2];                       /* { dg-message "declared here" } */

  int *p = g3;
  if (i < -1)
    p = a;
  else if (i < 0)
    p = b;
  else if (1 < i)
    p = g1;
  else if (0 < i)
    p = g2;

  p += j;
  sink (p);

  return p;         /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
return_3_globals_2_locals_varoff (int i, int j)
{
  int a[1];         /* { dg-message "declared here" } */
  int b[2];         /* { dg-message "declared here" } */

  int *p = g3;
  if (i < -1)
    p = a;
  else if (i < 0)
    p = b;
  else if (1 < i)
    p = g1;
  else if (0 < i)
    p = g2;

  p += j;
  sink (p);

  return p;         /* { dg-warning "function may return address of local" } */
}

