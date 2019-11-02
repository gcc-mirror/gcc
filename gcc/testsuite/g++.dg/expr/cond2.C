struct Term { };
struct Boolean : Term {
  explicit Boolean(bool);
};
struct IsZero : Term {
  Term *eval();
};
Term*
IsZero::eval()
{
  return true ? new Boolean(false) : this; // { dg-error "15:conditional expression" }
}
