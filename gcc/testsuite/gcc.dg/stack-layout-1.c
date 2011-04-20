/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing -fdump-rtl-expand" } */
union U {
  int a;
  float b;
};
struct A {
  union U u1;
  char a[100];
};
void bar (struct A *);
void foo ()
  {
    {
      struct A a;
      bar (&a);
    }
    {
      struct A a;
      bar (&a);
    }
  }

/* { dg-final { scan-rtl-dump-times "Partition" 1 "expand" } } */
/* { dg-final { cleanup-rtl-dump "expand" } } */
