/* PR c/71924 - missing -Wreturn-local-addr returning alloca result
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#define ATTR(...) __attribute__ ((__VA_ARGS__))

struct A { int a, b, c; };
struct B { int a, b, c[]; };

void sink (void*, ...);

ATTR (noipa) void*
return_alloca (int n)
{
  void *p = __builtin_alloca (n);
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_alloca_index_cst (int n)
{
  int *p = (int*)__builtin_alloca (n);
  p = &p[1];
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_alloca_plus_cst (int n)
{
  int *p = (int*)__builtin_alloca (n);
  p += 1;
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_alloca_plus_var (int n, int i)
{
  char *p = (char*)__builtin_alloca (n);
  p += i;
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_alloca_member_1 (int n)
{
  struct A *p = (struct A*)__builtin_alloca (n);
  sink (&p->a);
  return &p->a;     /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_alloca_member_2 (int n)
{
  struct A *p = (struct A*)__builtin_alloca (n);
  sink (&p->b);
  return &p->b;     /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_alloca_flexarray (int n)
{
  struct B *p = (struct B*)__builtin_alloca (n);
  sink (p->c);
  return p->c;      /* { dg-warning "function returns address of local" } */
}


ATTR (noipa) void*
return_array (void)
{
  int a[32];
  void *p = a;
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_array_index_cst (void)
{
  int a[32];
  void *p = &a[2];
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_array_plus_cst (void)
{
  int a[32];
  void *p = a + 2;
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_array_plus_var (int i)
{
  int a[32];
  void *p = a + i;
  sink (p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_array_member_1 (void)
{
  struct A a[2];
  int *p = &a[1].a;
  sink (a, p);
  return p;         /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_array_member_2 (void)
{
  struct A a[32];
  int *p = &a[1].b;
  sink (a, p);
  return p;         /* { dg-warning "function returns address of local" } */
}


ATTR (noipa) void*
return_vla (int n)
{
  char a[n];
  void *p = a;
  sink (p);
  return p;   /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_vla_index_cst (int n)
{
  char a[n];
  char *p = &a[3];
  sink (p);
  return p;   /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_vla_plus_cst (int n)
{
  char a[n];
  char *p = a + 3;
  sink (p);
  return p;   /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_vla_index_var (int n, int i)
{
  char a[n];
  char *p = &a[i];
  sink (p);
  return p;   /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_vla_plus_var (int n, int i)
{
  char a[n];
  char *p = a + i;
  sink (p);
  return p;   /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_vla_member_1 (int n, int i)
{
  struct A a[n];
  void *p = &a[i].a;
  sink (a, p);
  return p;   /* { dg-warning "function returns address of local" } */
}

ATTR (noipa) void*
return_vla_member_2 (int n, int i)
{
  struct A a[n];
  void *p = &a[i].b;
  sink (a, p);
  return p;   /* { dg-warning "function returns address of local" } */
}


ATTR (noipa) void*
return_alloca_or_alloca (int n, int i)
{
  void *p = i ? __builtin_alloca (n * i) : __builtin_alloca (n);
  sink (p);
  /* The warning here should really be "function returns".  */
  return p;   /* { dg-warning "function (returns|may return) address of local" } */
}

ATTR (noipa) void*
return_alloca_or_alloca_2 (int n, int i)
{
  void *p0 = __builtin_alloca (n);
  void *p1 = __builtin_alloca (n * 2);
  void *p = i ? p0 : p1;
  sink (p0, p1, p);
  /* Same as above.  */
  return p;   /* { dg-warning "function (returns|may return) address of local" } */
}

ATTR (noipa) void*
return_array_or_array (int i)
{
  int a[5];
  int b[7];
  void *p = i ? a : b;
  sink (a, b, p);
  /* The warning here should really be "function returns".  */
  return p;   /* { dg-warning "function (returns|may return) address of local" } */
}

ATTR (noipa) void*
return_array_or_array_plus_var (int i, int j)
{
  int a[5];
  int b[7];

  void *p0 = a + i;
  void *p1 = b + j;

  void *p = i < j ? p0 : p1;
  sink (a, b, p0, p1, p);
  /* The warning here should really be "function returns".  */
  return p;   /* { dg-warning "function (returns|may return) address of local" } */
}

extern int global[32];

ATTR (noipa) void*
may_return_global_or_alloca (int n, int i)
{
  void *p = i ? global : __builtin_alloca (n);
  sink (p);
  return p;   /* { dg-warning "function may return address of local" } */
}


ATTR (noipa) void*
may_return_global_or_alloca_plus_cst (int n, int i)
{
  int *p = i ? global : (int*)__builtin_alloca (n);
  p += 7;
  sink (p);
  return p;   /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
may_return_global_or_array (int n, int i)
{
  int a[32];
  void *p = i ? global : a;
  sink (p);
  return p;   /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
may_return_global_or_array_plus_cst (int n, int i)
{
  int a[32];
  int *p = i ? global : a;
  p += 4;
  sink (p);
  return p;   /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
may_return_global_or_vla (int n, int i)
{
  int a[n];
  void *p = i ? global : a;
  sink (p);
  return p;   /* { dg-warning "function may return address of local" } */
}

ATTR (noipa) void*
may_return_global_or_vla_plus_cst (int n, int i)
{
  int a[n];
  int *p = i ? global : a;
  p += 4;
  sink (p);
  return p;   /* { dg-warning "function may return address of local" } */
}
