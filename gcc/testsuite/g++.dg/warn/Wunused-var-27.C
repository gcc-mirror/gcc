// PR c++/82728
// { dg-do compile }
// { dg-options "-Wunused-but-set-variable" }

void
foo ()
{
  const int i = 1;		// { dg-bogus "set but not used" }
  switch (0)
    {
    case i:
      break;
    }
}
