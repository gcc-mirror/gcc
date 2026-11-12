/* { dg-do run } */

#pragma scalar_storage_order big-endian
struct B { _Complex unsigned c; };
#pragma scalar_storage_order default
struct B g[32];
int n = 4;
int main()
{
  for (int i = 0; i < n; i++) __imag__ g[i].c = i;
  if (__imag__ g[2].c != 2)
    __builtin_abort ();
  return 0;
}
