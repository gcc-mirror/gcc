class foo // { dg-message "'class foo' defined here" }
{
public:
  static void test (int i, int j, void *ptr, int k); // { dg-line decl }
};

// Wrong "const"-ness of a param, for static member fn (param 3).
void foo::test (int i, int j, const void *ptr, int k) // { dg-line defn }
{
}

// { dg-error "6: no declaration matches" "error" { target *-*-* } defn }
// { dg-message "15: candidate is: " "candidate is" { target *-*-* } decl }
// { dg-message "41: parameter 3 of candidate has type 'void\\*'" "param of decl" { target *-*-* } decl }
// { dg-message "43: \\.\\.\\.which does not match type 'const void\\*'" "param of defn" { target *-*-* } defn }
