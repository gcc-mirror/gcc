// PR c++/89785
// { dg-do compile { target c++14 } }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

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
  throw 42;	// { dg-error "is not a constant expression" "" { target c++20_down } }
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
      throw -42;	// { dg-error "is not a constant expression" "" { target c++20_down } }
    }
  while (0);
  return x;
}
