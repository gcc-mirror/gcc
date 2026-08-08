/* PR tree-optimization/126729 */
/* { dg-do run } */

struct S0 { int a; int v; } __attribute__((scalar_storage_order("little-endian")));
union U0 { struct S0 s; int i[2]; } __attribute__((scalar_storage_order("little-endian")));
struct S1 { int a; int v; } __attribute__((scalar_storage_order("big-endian")));
union U1 { struct S1 s; int i[2]; } __attribute__((scalar_storage_order("big-endian")));


__attribute__((noinline))
int f(void *a, bool b, bool bb)
{
  if (b)
  {
    union U0 t = *((union U0*)a);
    if (bb)
      return t.i[0];
    return t.s.a;
  }
  {
    union U1 t = *((union U1*)a);
    if (bb)
      return t.i[0];
    return t.s.a;
  }
}

__attribute__((noinline))
int f2(void *a, bool b, bool bb)
{
  if (b)
  {
    union U1 t = *((union U1*)a);
    if (bb)
      return t.i[0];
    return t.s.a;
  }
  {
    union U1 t = *((union U1*)a);
    if (bb)
      return t.i[0];
    return t.s.a;
  }
}

int main()
{
  union U1 a;
  union U0 b;
  int t = 0xabcd;
  a.s.a = t;
  b.s.a = t;
  if (f((void*)&a, 0, 0) != t)
    __builtin_abort();
  if (f((void*)&b, 1, 0) != t)
    __builtin_abort();
  if (f2((void*)&a, 1, 0) != t)
    __builtin_abort();
  if (f2((void*)&a, 0, 0) != t)
    __builtin_abort();
}
