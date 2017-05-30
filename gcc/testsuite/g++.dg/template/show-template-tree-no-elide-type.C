// { dg-options "-fdiagnostics-show-template-tree -fno-elide-type" }

template<typename> struct vector {};
template<typename, typename> struct map {};

void fn_1(vector<int>);
void fn_2(map<int, int>);
void fn_3(vector<map<int, float> >);

void test ()
{
  fn_1 (vector<double> ()); // { dg-error "could not convert .* from 'vector<double>' to 'vector<int>'" }
  /* { dg-begin-multiline-output "" }
  vector<
    [double != int]>
     { dg-end-multiline-output "" } */

  fn_2 (map<int, double>());  // { dg-error "could not convert .* from 'map<int,double>' to 'map<int,int>'" }
  /* { dg-begin-multiline-output "" }
  map<
    int,
    [double != int]>
     { dg-end-multiline-output "" } */
}
