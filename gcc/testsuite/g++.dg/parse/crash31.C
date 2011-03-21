struct A // { dg-error "forward declaration" }
{
  A : A; // { dg-error "expected|incomplete" }
  A : B; // { dg-error "not declared|incomplete" }
  A : A(); // { dg-error "undefined type|incomplete" }
  A : B(); // { dg-error "function call|incomplete|not declared" }
  A : A[]; // { dg-error "expected|array reference|incomplete" }
  A : B[]; // { dg-error "not declared|expected|array reference|incomplete" }
};
