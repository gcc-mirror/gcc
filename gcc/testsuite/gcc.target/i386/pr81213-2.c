/* { dg-require-ifunc "" } */
__attribute__((target_clones("avx","arch=slm","arch=core-avx2","default")))
static int
foo ()
{
  return 2;
}

int bar()
{
  return foo();
}
