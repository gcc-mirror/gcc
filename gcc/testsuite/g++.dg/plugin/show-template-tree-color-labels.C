/* Verify colorization of the labels in diagnostic-show-locus.c
   for template comparisons.
   Doing so requires a plugin; see the comments in the plugin for the
   rationale.  */

// { dg-options "-fdiagnostics-color=always -fdiagnostics-show-caret" }

template<typename> struct vector {};
template<typename, typename> struct map {};

void fn_1(vector<int>);
void fn_2(map<int, int>);

void test_1 (vector<double> vec)
{
  fn_1 (vec);
  /* { dg-begin-multiline-output "" }
could not convert '[01m[Kvec[m[K' from '[01m[Kvector<[01;32m[Kdouble[m[K>[m[K' to '[01m[Kvector<[01;34m[Kint[m[K>[m[K'
   fn_1 ([01;32m[Kv[m[K[01;32m[Ke[m[K[01;32m[Kc[m[K);
         [01;32m[K^[m[K[01;32m[K~[m[K[01;32m[K~[m[K
         [01;32m[K|[m[K
         [01;32m[Kvector<double>[m[K
     { dg-end-multiline-output "" } */
  // TODO: we don't yet highlight the mismatching part with color
}

void test_2 (const map<int, double> &m)
{
  fn_2 (m);
  /* { dg-begin-multiline-output "" }
could not convert '[01m[Km[m[K' from '[01m[Kmap<[...],[01;32m[Kdouble[m[K>[m[K' to '[01m[Kmap<[...],[01;34m[Kint[m[K>[m[K'
   fn_2 ([01;32m[Km[m[K);
         [01;32m[K^[m[K
         [01;32m[K|[m[K
         [01;32m[Kmap<[...],double>[m[K
     { dg-end-multiline-output "" } */
  // TODO: we don't yet highlight the mismatching part with color
}
