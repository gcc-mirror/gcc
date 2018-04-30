// { dg-do compile }
// { dg-additional-options "-g" }

class a
{
  bool b ();
};
bool
a::b ()
{
#pragma omp parallel
  ;

  return true;
}
