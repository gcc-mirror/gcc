// PR c++/89785
// { dg-do compile { target c++14 } }

constexpr int
foo (int x)
{
  switch (x)
    {
    case 0:
      break;
    case 2:
      break;
    }
  throw 42;	// { dg-error "is not a constant expression" }
  return 0;
}

constexpr int
bar (int x)
{
  do
    {
      switch (x)
	{
	case 0:
	  throw 42;
	case 1:
	  for (int i = 0; i < 10; i++)
	    continue;
	  break;
	}
      throw -42;	// { dg-error "is not a constant expression" }
    }
  while (0);
  return x;
}
