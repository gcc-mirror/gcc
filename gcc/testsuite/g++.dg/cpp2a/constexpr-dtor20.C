// PR c++/127046
// { dg-do run { target c++20 } }

struct T {
  char padding[4] = { 'x' };
  constexpr ~T () {}
};

constexpr bool
test ()
{
  T array[3] = {};
  return (array[0].padding[0] == 'x'
	  && array[1].padding[0] == 'x'
	  && array[2].padding[0] == 'x');
}

static_assert (test ());

bool (*volatile fp) () = test;

int
main ()
{
  if (!fp ())
    __builtin_abort ();
}
