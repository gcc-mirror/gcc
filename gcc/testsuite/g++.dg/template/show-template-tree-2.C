// Tests of -fdiagnostics-show-template-tree with variadic templates
// { dg-options "-fdiagnostics-show-template-tree -std=c++11" }

template<typename> struct vector {};
template<typename, typename> struct map {};
template<typename ... Types> struct var {};

void fn_0(var<>);
void fn_1(var<int>);
void fn_2(var<int, int>);
void fn_3(vector<var<> >);
void fn_4(vector<var<int> >);
void fn_5(vector<var<int, int> >);

void test_fn_0 ()
{
  fn_0 (var<> ());
  fn_0 (var<int> ()); // { dg-error "could not convert .* from 'var<int>' to 'var<>'" }
  /* { dg-begin-multiline-output "" }
  var<
    [int != ]>
     { dg-end-multiline-output "" } */
  fn_0 (var<int, int> ()); // { dg-error "could not convert .* from 'var<int, int>' to 'var<>'" }
  /* { dg-begin-multiline-output "" }
  var<
    [int, int != ]>
     { dg-end-multiline-output "" } */
  fn_0 (vector<var<int> >()); // { dg-error "could not convert .* from 'vector<var<int> >' to 'var<>'" }
  fn_0 (vector<var<int, int> >());  // { dg-error "could not convert .* from 'vector<var<int, int> >' to 'var<>'" }
}

void test_fn_1 ()
{
  fn_1 (var<> ()); // { dg-error "could not convert .* from 'var<>' to 'var<int>'" }
  /* { dg-begin-multiline-output "" }
  var<
    [ != int]>
     { dg-end-multiline-output "" } */
  fn_1 (var<int> ());
  fn_1 (var<int, int> ()); // { dg-error "could not convert .* from 'var<int, int>' to 'var<int>'" }
  /* { dg-begin-multiline-output "" }
  var<
    [int, int != int]>
     { dg-end-multiline-output "" } */
  fn_1 (vector<var<int> >()); // { dg-error "could not convert .* from 'vector<var<int> >' to 'var<int>'" }
  fn_1 (vector<var<int, int> >()); // { dg-error "could not convert .* from 'vector<var<int, int> >' to 'var<int>'" }
}

void test_fn_2 ()
{
  fn_2 (var<> ()); // { dg-error "could not convert .* from 'var<>' to 'var<int, int>'" }
  /* { dg-begin-multiline-output "" }
  var<
    [ != int, int]>
     { dg-end-multiline-output "" } */
  fn_2 (var<int> ()); // { dg-error "could not convert .* from 'var<int>' to 'var<int, int>'" }
  /* { dg-begin-multiline-output "" }
  var<
    [int != int, int]>
     { dg-end-multiline-output "" } */
  fn_2 (var<int, int> ());
  fn_2 (vector<var<int> >()); // { dg-error "could not convert .* from 'vector<var<int> >' to 'var<int, int>'" }
  fn_2 (vector<var<int, int> >()); // { dg-error "could not convert .* from 'vector<var<int, int> >' to 'var<int, int>'" }
}

void test_fn_3 ()
{
  fn_3 (var<> ()); // { dg-error "could not convert .* from 'var<>' to 'vector<var<> >'" }
  fn_3 (var<int> ()); // { dg-error "could not convert .* from 'var<int>' to 'vector<var<> >'" }
  fn_3 (var<int, int> ()); // { dg-error "could not convert .* from 'var<int, int>' to 'vector<var<> >'" }
  fn_3 (vector<var<> >());
  fn_3 (vector<var<int> >());  // { dg-error "could not convert .* from 'vector<var<int>>' to 'vector<var<>>'" }
  /* { dg-begin-multiline-output "" }
  vector<
    var<
      [int != ]>>
     { dg-end-multiline-output "" } */
  fn_3 (vector<var<int, int> >()); // { dg-error "could not convert .* from 'vector<var<int, int>>' to 'vector<var<>>'" }
  /* { dg-begin-multiline-output "" }
  vector<
    var<
      [int, int != ]>>
     { dg-end-multiline-output "" } */
}

void test_fn_4 ()
{
  fn_4 (var<> ()); // { dg-error "could not convert .* from 'var<>' to 'vector<var<int> >'" }
  fn_4 (var<int> ()); // { dg-error "could not convert .* from 'var<int>' to 'vector<var<int> >'" }
  fn_4 (var<int, int> ()); // { dg-error "could not convert .* from 'var<int, int>' to 'vector<var<int> >'" }
  fn_4 (vector<var<> >()); // { dg-error "could not convert .* from 'vector<var<>>' to 'vector<var<int>>'" }
  /* { dg-begin-multiline-output "" }
  vector<
    var<
      [ != int]>>
     { dg-end-multiline-output "" } */
  fn_4 (vector<var<int> >()); 
  fn_4 (vector<var<int, int> >()); // { dg-error "could not convert .* from 'vector<var<int, int>>' to 'vector<var<int>>'" }
  /* { dg-begin-multiline-output "" }
  vector<
    var<
      [int, int != int]>>
     { dg-end-multiline-output "" } */
}

void test_fn_5 ()
{
  fn_5 (var<> ()); // { dg-error "could not convert .* from 'var<>' to 'vector<var<int, int> >'" }
  fn_5 (var<int> ());  // { dg-error "could not convert .* from 'var<int>' to 'vector<var<int, int> >'" }
  fn_5 (var<int, int> ());  // { dg-error "could not convert .* from 'var<int, int>' to 'vector<var<int, int> >'" }
  fn_5 (vector<var<int> >());  // { dg-error "could not convert .* from 'vector<var<int>>' to 'vector<var<int, int>>'" }
  /* { dg-begin-multiline-output "" }
  vector<
    var<
      [int != int, int]>>
     { dg-end-multiline-output "" } */
  fn_5 (vector<var<int, int> >());
}
