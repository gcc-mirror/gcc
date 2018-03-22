// PR c++/71386
// { dg-do run { target c++14 } }

template<class...XS>
auto List(XS...xs)
{
  return [=](auto processList){return processList(xs...);};
}

auto l1 = List(42);

int test (int a)
{
  if (a != 42)
    __builtin_abort ();
  return 0;
}

auto foo = [](auto... xs1)
  {
    return [=]()
    { 
      return l1([=](auto)
      {
	return test (xs1...);
      });
    };
  };

int main()
{
  auto concat = l1(foo);
  concat();
}
