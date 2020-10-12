/* PR middle-end/92349 - ICE in -Warray-bounds on a VLA member
   { dg-do compile }
   { dg-options "-O2 -Wall" }
   { dg-require-effective-target alloca } */

typedef __SIZE_TYPE__ size_t;

void sink (void*, ...);

void mem_vla_cst_store_idx (void)
{
  int n = 3;

  struct {
    char a[n], b;
  } s;

  char *p = s.a;

  s.a[0] = 0;
  s.b = 0;

  *++p = 1;
  *++p = 2;

  sink (&s, p);
}

void mem_vla_range_store_idx (int n)
{
  if (n < 3 || 4 < n)
    n = 3;

  struct {
    char a[n], b;
  } s;

  char *p = s.a;

  s.a[0] = 0;
  s.b = 0;

  *++p = 1;
  *++p = 2;

  sink (&s, p);
}

void mem_vla_var_store_idx (size_t n)
{
  struct {
    char a[n], b;
  } s;

  char *p = s.a;

  s.a[0] = 0;
  s.b = 0;

  *++p = 1;
  *++p = 2;

  sink (&s, p);
}


void mem_vla_cst_store_ptr (void)
{
  int n = 3;

  struct {
    char a[n], b;
  } s;

  char *p = s.a;

  *p++ = __LINE__;
  *p++ = __LINE__;
  *p++ = __LINE__;

  sink (&s, p);
}

void mem_vla_range_store_ptr (int n)
{
  if (n < 3 || 4 < n)
    n = 3;

  struct {
    char a[n], b;
  } s;

  char *p = s.a;

  *p++ = __LINE__;
  *p++ = __LINE__;
  *p++ = __LINE__;

  sink (&s, p);
}

void mem_vla_var_store_ptr (size_t n)
{
  struct {
    char a[n], b;
  } s;

  char *p = s.a;

  *p++ = __LINE__;
  *p++ = __LINE__;
  *p++ = __LINE__;

  sink (&s, p);
}
