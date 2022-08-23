/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

class C
{
public:
  static int static_member;
  virtual void f() {}
};

extern C v[];

int
main ()
{
#pragma omp target map(v) /* { dg-error ".v. does not have a mappable type in .map. clause" } */
  {
  }
}
