class bar;

class foo // { dg-message "'class foo' defined here" }
{
public:
  void test (int i, foo *j, int k); // { dg-line decl }
};

// Missing '*' on a param (param 2).
void foo::test (int i, foo j, int k) // { dg-line defn }
{
}

// { dg-error "6: no declaration matches" "error" { target *-*-* } defn }
// { dg-message "8: candidate is: " "candidate is" { target *-*-* } decl }
// { dg-message "26: parameter 2 of candidate has type 'foo\\*'" "param of decl" { target *-*-* } decl }
// { dg-message "28: \\.\\.\\.which does not match type 'foo'" "param of defn" { target *-*-* } defn }
