int foo(__complex__ int z0, __complex__ int z1)
{
  return z0 != 0 || z1 != 0;
}

int foo1(__complex__ int z0, __complex__ int z1)
{
  return z0 == 0 && z1 == 0;
}
