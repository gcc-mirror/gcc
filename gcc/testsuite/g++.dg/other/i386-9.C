// PR target/48142
// Testcase by Zdenek Sojka <zsojka@seznam.cz>

// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-options "-Os -mpreferred-stack-boundary=5 -fstack-check -fno-omit-frame-pointer" }

int main()
{
  try { throw 0; }
  catch (...) {}
  return 0;
}
