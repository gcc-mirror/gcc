class foo // { dg-message "'class foo' defined here" }
{
public:
  void test (int i, int j, void *ptr, int k); // { dg-line close_decl }
  void test (int i, int j, int k);            // { dg-line other_decl }
};

// Wrong "const"-ness of a param, for one of the overloads (param 3).
void foo::test (int i, int j, const void *ptr, int k) // { dg-line defn }
{
}

// { dg-error "6: no declaration matches" "error" { target *-*-* } defn }
// { dg-message "8: candidate 1: " "candidate 1" { target *-*-* } other_decl }
// { dg-message "8: candidate 2: " "candidate 2" { target *-*-* } close_decl }
