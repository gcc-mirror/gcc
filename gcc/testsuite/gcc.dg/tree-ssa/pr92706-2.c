/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-esra" } */

typedef __UINT64_TYPE__ uint64_t;
typedef __UINT32_TYPE__ uint32_t;
struct S { uint32_t i[2]; } __attribute__((aligned(__alignof__(uint64_t))));
typedef uint64_t my_int64 __attribute__((may_alias));
uint64_t load (void *p)
{
  struct S u, v, w;
  uint64_t tem;
  tem = *(my_int64 *)p;
  *(my_int64 *)&v = tem;
  u = v;
  w = u;
  return *(my_int64 *)&w;
}

/* { dg-final { scan-tree-dump "Created a replacement for v" "esra" } } */
