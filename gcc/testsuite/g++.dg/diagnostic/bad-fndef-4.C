// { dg-additional-options "-fdiagnostics-show-caret" }

class foo // { dg-message "'class foo' defined here" }
{
public:
  void test (int i, int j, void *ptr, int k); // { dg-line decl }
};

// Wrong "const"-ness of a param (param 3).
void foo::test (int i, int j, const void *ptr, int k) // { dg-line defn }
{
}

// { dg-error "6: no declaration matches" "error" { target *-*-* } defn }
/* { dg-begin-multiline-output "" }
 void foo::test (int i, int j, const void *ptr, int k)
      ^~~
   { dg-end-multiline-output "" } */
// { dg-message "8: candidate is: " "candidate is" { target *-*-* } decl }
/* { dg-begin-multiline-output "" }
   void test (int i, int j, void *ptr, int k);
        ^~~~
   { dg-end-multiline-output "" } */
// { dg-message "34: parameter 3 of candidate has type 'void\\*'" "param of decl" { target *-*-* } decl }
/* { dg-begin-multiline-output "" }
   void test (int i, int j, void *ptr, int k);
                            ~~~~~~^~~
   { dg-end-multiline-output "" } */
// { dg-message "43: \\.\\.\\.which does not match type 'const void\\*'" "param of defn" { target *-*-* } defn }
/* { dg-begin-multiline-output "" }
 void foo::test (int i, int j, const void *ptr, int k)
                               ~~~~~~~~~~~~^~~
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
 class foo
       ^~~
   { dg-end-multiline-output "" } */
