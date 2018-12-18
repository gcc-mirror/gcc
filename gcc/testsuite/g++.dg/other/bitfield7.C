typedef int __attribute__((warn_if_not_aligned(8))) intwna;

struct S
{
  intwna : 2;  // { dg-error "cannot declare bit-field" }
  intwna i : 2;  // { dg-error "10:cannot declare bit-field .i." }
};
