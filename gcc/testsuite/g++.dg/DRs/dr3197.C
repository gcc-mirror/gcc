// CWG 3197
// { dg-do run }

#include <cassert>
#include <cstring>

int main()
{
  int i = 0;
  unsigned char a[sizeof(int)] = {};
  assert(memcmp(&i, a, sizeof(i)) == 0);   // guaranteed to hold in C
}
