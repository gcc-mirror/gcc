/* { dg-do run } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("arch=x86-64", "arch=x86-64-v2", "arch=x86-64-v3", "arch=x86-64-v4", "default")))
int
foo ()
{
  return 0;
}

int
main ()
{
  return foo ();
}
