// [over.best.ics]: For the purpose of ranking implicit conversion
// sequences as described in _over.ics.rank_, the ambiguous conversion
// sequence is treated as a user-defined sequence that is indistinguishable
// from any other user- defined conversion sequence.

struct A
{
  A(long);
  A(char);
};

struct B
{
  B(int);
};

void f(A);			// { dg-error "" "candidate" }
void f(B);			// { dg-error "" "candidate" }

int main()
{
  f (42);			// { dg-error "ambiguous" "ambiguous" }
}
