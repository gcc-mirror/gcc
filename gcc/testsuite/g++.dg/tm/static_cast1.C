// The inverse of a transaction-safety conversion cannot be performed with
// static_cast.
// { dg-options "-fgnu-tm" }

typedef void (*TS)() transaction_safe;
void f()
{
  static_cast<TS>(f); // { dg-error "static_cast" }
}
