/* PR c++/87541 - ICE using a constant decl as an attribute alloc_size argument
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds" } */

#define ALLOC_SIZE(N)   __attribute__ ((alloc_size (N)))

const int i1 = 1;
ALLOC_SIZE (i1) void* fcst (int);

void* call_fcst (void)
{
  void *p = fcst (1);
  __builtin___memset_chk (p, 0, 2, __builtin_object_size (p, 1));   // { dg-warning "\\\[-Wstringop-overflow=" }
  return p;
}


enum { e1 = 1 };
ALLOC_SIZE (e1) void* fenum (int);

void* call_fenum (void)
{
  void *p = fenum (1);
  __builtin___memset_chk (p, 0, 2, __builtin_object_size (p, 1));   // { dg-warning "\\\[-Wstringop-overflow=" }
  return p;
}


template <class T>
struct A
{
  ALLOC_SIZE (T::N1) static void* ftemplarg_1 (int);
  ALLOC_SIZE (T::N2) static void*
  ftemplarg_2 (int); // { dg-warning "attribute argument value .2. exceeds the number of function parameters 1" }
};

struct B { static const int N1 = 1; static const int N2 = 1; };

void* call_ftemplarg_1 (A<B> *pa)
{
  void *p = pa->ftemplarg_1 (1);
  __builtin___memset_chk (p, 0, 2, __builtin_object_size (p, 1));   // { dg-warning "\\\[-Wstringop-overflow=" }
  return p;
}

struct C { static const int N1 = 1; static const int N2 = 2; };

void* call_ftemplarg_2 (A<C> *pa)
{
  void *p = pa->ftemplarg_2 (1);
  __builtin___memset_chk (p, 0, 2, __builtin_object_size (p, 1));
  return p;
}
