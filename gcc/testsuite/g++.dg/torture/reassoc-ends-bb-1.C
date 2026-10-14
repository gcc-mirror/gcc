// A call that can throw ends its basic block.  A range test on its
// result must not be inserted after it.
// { dg-do compile }

struct S { ~S (); };
bool cmp ();

unsigned
f ()
{
  S guard;
  unsigned s = 0;
  int m = 0;
  bool c = cmp ();
  for (int i = 0; i < 4; i++)
    {
      m = -(int) c;
      s = s * 5u + (m ? 0u : 3u);
    }
  return s + (m != -1 || c);
}
