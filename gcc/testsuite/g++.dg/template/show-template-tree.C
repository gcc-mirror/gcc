// { dg-options "-fdiagnostics-show-template-tree" }

template<typename> struct vector {};
template<typename, typename> struct map {};
template<int> struct arr {};

void fn_1(vector<int>);
void fn_2(map<int, int>);
void fn_3(vector<map<int, float> >);
void takes_arr_10 (arr<10>);

void test ()
{
  fn_1 (vector<int> ());
  fn_1 (42); // { dg-error "could not convert '42' from 'int' to 'vector<int>'" }
  fn_1 (vector<double> ()); // { dg-error "could not convert .* from 'vector<double>' to 'vector<int>'" }
  /* { dg-begin-multiline-output "" }
  vector<
    [double != int]>
     { dg-end-multiline-output "" } */
  fn_1 (map<int, int> ()); // { dg-error "could not convert .* from 'map<int, int>' to 'vector<int>'" }

  fn_2 (map<int, int>());
  fn_2 (map<int, double>());  // { dg-error "could not convert .* from 'map<.\\.\\.\\..,double>. to .map<.\\.\\.\\..,int>'" }
  /* { dg-begin-multiline-output "" }
  map<
    [...],
    [double != int]>
     { dg-end-multiline-output "" } */
  fn_2 (map<double, double>());  // { dg-error "could not convert .* from .map<double,double>. to .map<int,int>." }
  /* { dg-begin-multiline-output "" }
  map<
    [double != int],
    [double != int]>
     { dg-end-multiline-output "" } */

  fn_3 (vector<map<int, float> >());
  fn_3 (vector<map<int, double> >());  // { dg-error "could not convert .* from 'vector<map<.\\.\\.\\..,double>>' to 'vector<map<.\\.\\.\\..,float>>'" }
  /* { dg-begin-multiline-output "" }
  vector<
    map<
      [...],
      [double != float]>>
     { dg-end-multiline-output "" } */

  takes_arr_10 (arr<5>()); // { dg-error "could not convert '.*' from 'arr<5>' to 'arr<10>'" }
  /* { dg-begin-multiline-output "" }
  arr<
    [5 != 10]>
     { dg-end-multiline-output "" } */
}
