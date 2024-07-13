/* Verify colorization of the output of -fdiagnostics-show-template-tree,
   and within the %H and %I format codes.
   Doing so requires a plugin; see the comments in the plugin for the
   rationale.  */

// { dg-options "-fdiagnostics-show-template-tree -fdiagnostics-color=always" }

template<typename> struct vector {};
template<typename, typename> struct map {};

void fn_1(vector<int>);
void fn_2(map<int, int>);

void test ()
{
  fn_1 (vector<double> ());
  /* { dg-begin-multiline-output "" }
could not convert '[01m[Kvector<double>()[m[K' from '[01m[Kvector<[01;32m[Kdouble[m[K>[m[K' to '[01m[Kvector<[01;34m[Kint[m[K>[m[K'
  vector<
    [[01;32m[Kdouble[m[K != [01;34m[Kint[m[K]>
     { dg-end-multiline-output "" } */

  fn_2 (map<int, double>());
  /* { dg-begin-multiline-output "" }
could not convert '[01m[Kmap<int, double>()[m[K' from '[01m[Kmap<[...],[01;32m[Kdouble[m[K>[m[K' to '[01m[Kmap<[...],[01;34m[Kint[m[K>[m[K'
  map<
    [...],
    [[01;32m[Kdouble[m[K != [01;34m[Kint[m[K]>
     { dg-end-multiline-output "" } */
}
