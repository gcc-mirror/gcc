/* End-to-end test of -fdiagnostics-show-template-tree and -felide-type
   using the STL.
   In particular, ensure that we don't print the default arguments e.g.
   rather than printing
     from 'vector<double,allocator<double>>' to 'vector<float,allocator<float>>'
   (albeit with differences nicely color-coded), we want to print:
     from 'vector<double>' to 'vector<float>'
   (again, with the "double" and "float" highlighted, though we can't test
   for that in this case).  */

// { dg-options "-fdiagnostics-show-template-tree -Wno-return-type" }

#include <map>
#include <vector>

using std::vector;
using std::map;

void takes_vf (vector<float> v);
void takes_mivf (map<int, vector<float> > v);

int test ()
{
  takes_vf (vector<double> ()); // { dg-error "could not convert '.*' from 'vector<double>' to 'vector<float>'" }
  /* { dg-begin-multiline-output "" }
  vector<
    [double != float]>
     { dg-end-multiline-output "" } */

  takes_mivf (map<int, vector<double> > ()); // { dg-error "could not convert '.*' from 'map<.\\.\\.\\..,vector<double>>' to 'map<.\\.\\.\\..,vector<float>>'" }
  /* { dg-begin-multiline-output "" }
  map<
    [...],
    vector<
      [double != float]>>
     { dg-end-multiline-output "" } */
}
