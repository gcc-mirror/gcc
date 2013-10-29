/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

/* <feature>
   A program is considered ill formed if the _Cilk_spawn form of this
    expression appears other than in one of the following contexts:
    as the entire body of an expression statement,
    as the entire right hand side of an assignment expression that is the entire
    body of an expression statement, or as the entire initializer-clause in a 
    simple declaration.
   </feature>
*/

int spawn_func (int arg)
{
  return arg + 1;
}

int check()
{
  int z;
  z = 23, _Cilk_spawn spawn_func (3), 3424; /* { dg-error "spawned function call cannot be part of a comma expression" } */
  23, spawn_func (5), _Cilk_spawn spawn_func (3); /* { dg-error "spawned function call cannot be part of a comma expression" } */
  _Cilk_spawn spawn_func (0), _Cilk_spawn spawn_func (3), 3, spawn_func (0); /* { dg-error "spawned function call cannot be part of a comma expression" } */
  return 23;
}
