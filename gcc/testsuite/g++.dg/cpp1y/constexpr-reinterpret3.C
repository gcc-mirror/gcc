// PR c++/113545
// { dg-do run { target c++14 } }

char foo;

// This one caught a call to gcc_unreachable in
// cp/constexpr.cc:label_matches, when passed a convert_expr from the
// cast in the call.
constexpr unsigned char swbar(__UINTPTR_TYPE__ baz)
{
  switch (baz)
    {
    case 13:
      return 11;
    case 14:
      return 78;
    case 2048:
      return 13;
    default:
      return 42;
    }
}

// For reference, the equivalent* if-statements.
constexpr unsigned char ifbar(__UINTPTR_TYPE__ baz)
{
  if (baz == 13)
    return 11;
  else if (baz == 14)
    return 78;
  else if (baz == 2048)
    return 13;
  else
    return 42;
}

__attribute__ ((__noipa__))
void xyzzy(int x)
{
  if (x != 42)
    __builtin_abort ();
}

int main()
{
  unsigned const char c = swbar(reinterpret_cast<__UINTPTR_TYPE__>(&foo));
  xyzzy(c);
  unsigned const char d = ifbar(reinterpret_cast<__UINTPTR_TYPE__>(&foo));
  xyzzy(d);
  unsigned const char e = swbar((__UINTPTR_TYPE__) &foo);
  xyzzy(e);
  unsigned const char f = ifbar((__UINTPTR_TYPE__) &foo);
  xyzzy(f);
}
