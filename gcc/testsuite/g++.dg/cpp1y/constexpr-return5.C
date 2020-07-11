// PR c++/86327
// { dg-do compile { target c++14 } }

int global = 0;

constexpr int
f (bool arg)
{
  if (arg)
    return 1;
  return global++;
}
