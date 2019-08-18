struct S
{
  static S();  // { dg-error "3:constructor" }
  static ~S();  // { dg-error "3:destructor" }
};
