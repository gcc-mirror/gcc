struct A
{
  static int a : 1;  // { dg-error "14:static member .a. cannot be a bit-field" }
};
