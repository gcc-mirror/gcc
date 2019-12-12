/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

class C /* { dg-message "type .C. with virtual members is not mappable" } */
{
public:
  static int static_member; /* { dg-message "static field .C::static_member. is not mappable" } */
  virtual void f() {}
};

extern C v[];

int
main ()
{
#pragma omp target map(v) /* { dg-error ".v. does not have a mappable type in .map. clause" } */
  /* { dg-message "incomplete type .C \\\[\\\]. is not mappable" "" { target *-*-* } .-1 } */
  {
  }
}
