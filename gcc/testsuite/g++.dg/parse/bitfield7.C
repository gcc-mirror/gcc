struct A
{
  friend int : 1;  // { dg-error "unnamed field" }
};
