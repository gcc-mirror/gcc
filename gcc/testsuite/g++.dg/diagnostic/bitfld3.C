struct S
{
  typedef int i : 3;  // { dg-error "15:cannot declare .i." }
  typedef int : 3;  // { dg-error "cannot declare" }
};
