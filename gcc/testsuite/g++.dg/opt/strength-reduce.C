// This testcase was miscompiled on s390x, because strength-reduction
// did not see biv in C::foo as used after loop, but it was used
// in a REG_EQUAL note.
// { dg-do run }
// { dg-options "-O2" }

extern "C" void abort (void);

struct C
{
  int foo (char ch, int offset = 0x7fffffff) const;
  int bar (int offset, char c) const;
  char *a;
};

int C::bar (int offset, char c) const
{
  char ch = a[offset];
  if (ch < c)
    return -1;
  if (ch > c)
    return 1;
  return 0;
}

int C::foo (char ch, int offset) const
{
  int len = __builtin_strlen (a);
  if (len == 0)
    return 0x7fffffff;
  if (offset >= len)
    offset = len - 1;

  while (bar (offset, ch) != 0)
    {
      if (offset == 0)
        return 0x7fffffff;
      offset--;
    }

  return offset;
}

int main (void)
{
  C c;
  c.a = "/some/dir/file.ext";
  if (c.foo ('/') != 9)
    abort ();
  return 0;
}
