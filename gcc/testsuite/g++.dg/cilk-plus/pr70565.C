// { dg-do compile } */
// { dg-options "-fcilkplus" }

int array[999];
void foo()
{
  _Cilk_for (int i=0; i < 999; ++i)
    array[:] = 0;
}
