class foo // { dg-message "'class foo' defined here" }
{
public:
  void test (int i, int j, const char *k, ...);            // { dg-line decl }
};

// Variadic vs non-variadic *and* a mismatching param

void foo::test (int i, int j, int k) // { dg-line defn }
{
}

// { dg-error "6: no declaration matches" "error" { target *-*-* } defn }
// { dg-message "8: candidate is: " "candidate is" { target *-*-* } decl }

// The candidate is too different from the decl for a "close match" hint:
// { dg-bogus "parameter 3 of candidate has type" "param mismatch" { target *-*-* } decl }
