// Test whether __func__ works for ordinary member functions.

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Matt Austern <austern@apple.com>, 3 Aug 2003
// { dg-do run }

struct y8a
{
  const char* zqjx(int, char);
};

const char* y8a::zqjx(int, char)
{
  return __func__;
}


int main()
{
  y8a tmp;
  const char* s = tmp.zqjx(16, 'x');
  bool ok = true;

  ok = ok && s[0] == 'z';
  ok = ok && s[1] == 'q';
  ok = ok && s[2] == 'j';
  ok = ok && s[3] == 'x';
  ok = ok && s[4] == '\0';

  return ok ? 0 : 1;
}
