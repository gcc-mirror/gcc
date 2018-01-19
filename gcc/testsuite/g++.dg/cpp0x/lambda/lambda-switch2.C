// PR c++/82728
// { dg-do compile { target c++11 } }

void
foo ()
{
  const int i = 1;

  [=]()
    {
      switch (0)
	{
	case i:
	  break;
	}
      static_assert (i, "i");
    };
}
