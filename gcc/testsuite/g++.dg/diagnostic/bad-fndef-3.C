class foo // { dg-message "'class foo' defined here" }
{
public:
  void test (int i, int j, int k) const; // { dg-line decl }
};

// Wrong "const"-ness of "this":

void foo::test (int i, int j, int k) // { dg-line defn }
{
}

// { dg-error "6: no declaration matches" "error" { target *-*-* } defn }
// { dg-message "8: candidate is: " "candidate is" { target *-*-* } decl }
// { dg-message "8: parameter 'this' of candidate has type 'const foo\\*'" "this of decl" { target *-*-* } decl }
// { dg-message "6: \\.\\.\\.which does not match type 'foo\\*'" "this of defn" { target *-*-* } defn }
