// Test whether __func__ works for namespace-scope C++ functions.

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Matt Austern <austern@apple.com>, 3 Aug 2003
// { dg-do run }

namespace xyzzy
{
  const char* ab6(double, void*)
  {
    return __func__;
  }
}

int main()
{
  const char* s = xyzzy::ab6(2.3, (void*) 0);
  bool ok = true;

  ok = ok && s[0] == 'a';
  ok = ok && s[1] == 'b';
  ok = ok && s[2] == '6';
  ok = ok && s[3] == '\0';

  return ok ? 0 : 1;
}
