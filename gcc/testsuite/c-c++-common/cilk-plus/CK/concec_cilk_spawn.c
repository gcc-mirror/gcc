/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

/* <feature> Consecutive _Cilk_spawn tokens are not permitted
   </feature>
*/

int spawn_func (int arg)
{
  return arg + 1;
}

void func ()
{
  int a;
  a = _Cilk_spawn _Cilk_spawn spawn_func (4); /* { dg-error "consecutive" } */
  a = _Cilk_spawn _Cilk_spawn _Cilk_spawn spawn_func (4); /* { dg-error "consecutive" } */
  a = _Cilk_spawn _Cilk_spawn _Cilk_spawn _Cilk_spawn spawn_func (4); /* { dg-error "consecutive" } */
  return;
}
