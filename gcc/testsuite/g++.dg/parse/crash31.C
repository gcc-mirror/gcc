struct A
{
  A : A; // { dg-error "" }
  A : B; // { dg-error "" }
  A : A(); // { dg-error "" }
  A : B(); // { dg-error "" }
  A : A[]; // { dg-error "" }
  A : B[]; // { dg-error "" }
};
