// PR c++/67376 - [5/6 regression] Comparison with pointer to past-the-end
//     of array fails inside constant expression
// { dg-do compile { target c++11 } }

int a [2];

constexpr const int* pa[] = {
  a,
  a + 0,
  a + 1,
  a + 2,
  &a [0],
  &a [0] + 0,
  &a [0] + 1,
  &a [0] + 2,
  &a [1],
  &a [1] - 1,
  &a [1] + 0,
  &a [1] + 1,
  &a [2] - 2,
  &a [2] - 1,
  &a [2] + 0
};

#define Assert(e) static_assert ((e), #e)

Assert (!(a == 0));
Assert (!(a == (int*)0));
Assert (!(a == nullptr));

Assert (a != 0);
Assert (a != (int*)0);
Assert (a != nullptr);

Assert (!(0 == a));
Assert (!((int*)0 == a));
Assert (!(nullptr == a));

Assert (0 != a);
Assert ((int*)0 != a);
Assert (nullptr != a);

bool constexpr test_eq (unsigned inx)
{
  return inx ? pa [inx - 1] == 0 && 0 == pa [inx - 1]
    && test_eq (inx - 1) : pa [inx] == 0 && 0 == pa [inx];
}

Assert (!test_eq (sizeof pa / sizeof *pa));

bool constexpr test_ne (unsigned inx)
{
  return inx ? pa [inx - 1] != 0 && 0 != pa [inx - 1]
    && test_ne (inx - 1) : pa [inx] != 0 && 0 != pa [inx];
}

Assert (test_ne (sizeof pa / sizeof *pa));
