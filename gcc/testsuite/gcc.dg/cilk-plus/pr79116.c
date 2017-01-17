/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int array[1024];
void foo()
{
  _Cilk_for (int i = 0; i < 512; ++i)
    array[:] = __sec_implicit_index(0);
}
