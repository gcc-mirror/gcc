// PR c++/40752
// { dg-additional-options -funsigned-char }

#pragma GCC diagnostic error "-Wsign-conversion"
void f(char *ar, int i)
{
  ar[i] -= 'a' - 'A';
}
