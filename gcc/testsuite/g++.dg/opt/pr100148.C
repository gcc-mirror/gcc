// PR rtl-optimization/100148
// { dg-do compile }
// { dg-options "-O2 -fno-dce -fno-tree-dce -fno-tree-dominator-opts -fno-tree-sink -fcompare-debug" }

int i;
enum E { } e, ee;

bool
baz (int)
{
  return ee;
}

bool bar ();
bool a, b;

void
foo ()
{
  switch (ee)
    {
    case 0:
      e = E (a ? : i);
    case 1:
      !(b || baz (0) && bar ());
    }
}
