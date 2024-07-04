/* PR target/71763 */
/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec } */

int main ()
{
  __builtin_vec_st (); /* { dg-error "too few arguments to function" } */

}

