// With -fnon-call-exceptions a comparison that can trap ends its basic
// block.  A range test on its result must not be inserted after it.
// { dg-do compile }
// { dg-additional-options "-fnon-call-exceptions" }

struct S { ~S (); };

unsigned
f (float a, float b)
{
  unsigned s = 0;
  int m = 0;
  bool c;
  {
    S guard;
    c = a > b;
  }
  for (int i = 0; i < 4; i++)
    {
      m = -(int) c;
      s = s * 5u + (m ? 0u : 3u);
    }
  return s + (m != -1 || c);
}
