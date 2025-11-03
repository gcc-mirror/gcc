// { dg-additional-options -fmodules }

import M;

int main()
{
  const char *const p = nullptr;
  A<char> (p, 0);
}
