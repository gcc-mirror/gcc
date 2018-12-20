/* { dg-do compile  { target { powerpc64*-*-* } } } */
/* { dg-options "-m32 -mcpu=power7 -O1 -fexpensive-optimizations --param ira-max-conflict-table-size=0 --param max-cse-insns=3 -c -mcpu=e300c3" } */

__attribute__((target_clones("cpu=power9,default")))
long mod_func (long a, long b)
{
  return a % b;
}

long mod_func_or (long a, long b, long c)
{
  return mod_func (a, b) | c;
}
