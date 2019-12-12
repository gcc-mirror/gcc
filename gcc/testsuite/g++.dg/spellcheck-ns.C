// { dg-options "-fdiagnostics-show-caret" }

namespace outer {
  namespace inner_ns {
  }
  typedef int some_typedef;
}

using namespace outer;
using namespace outer::inner_ms; // { dg-error "'inner_ms' is not a namespace-name; did you mean 'inner_ns'" }
/* { dg-begin-multiline-output "" }
 using namespace outer::inner_ms;
                        ^~~~~~~~
                        inner_ns
   { dg-end-multiline-output "" } */

outer::some_typedfe var; // { dg-error "'some_typedfe' in namespace 'outer' does not name a type; did you mean 'some_typedef'" }
/* { dg-begin-multiline-output "" }
 outer::some_typedfe var;
        ^~~~~~~~~~~~
        some_typedef
   { dg-end-multiline-output "" } */
