// { dg-lto-do link }
// { dg-lto-options { { -g -O2 -flto -flto-partition=none } } }

void __attribute__((externally_visible))
foo (int i)
{
}
