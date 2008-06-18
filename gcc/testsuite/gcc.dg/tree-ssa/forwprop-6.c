/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1 -W -Wall" } */
#if (__SIZEOF_INT__ == __SIZEOF_FLOAT__)
typedef int intflt;
#elif (__SIZEOF_LONG__ == __SIZEOF_FLOAT__)
typedef long intflt;
#else
#error Add target support here for type that will union float size
#endif

intflt b;
void f(void)
{
  float a;
  a = 1;
  b = *(intflt*)&a; /* { dg-warning "aliasing" } */
}

/* We should be able to convert the cast to a VCE in forwprop1,
   even if there is an aliasing violation. */
/* { dg-final { scan-tree-dump-times "VIEW_CONVERT_EXPR" 1 "forwprop1"} } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
