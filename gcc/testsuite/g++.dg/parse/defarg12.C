/* PR28266 This used to ICE in gimple_add_tmp_var */
/* { dg-do "compile" } */

struct A
{
  int i;
  A(int = X); /* { dg-error "was not declared in this scope" }*/
};

void foo()
{
  A().i;
}
