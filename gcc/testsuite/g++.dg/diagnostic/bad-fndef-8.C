class foo // { dg-message "'class foo' defined here" }
{
public:
  void test (int i, int j, int k);            // { dg-line decl }
};

// Variadic vs non-variadic

void foo::test (int i, int j, int k, ...) // { dg-line defn }
{
}

// { dg-error "6: no declaration matches" "error" { target *-*-* } defn }
// { dg-message "8: candidate is: " "candidate is" { target *-*-* } decl }
