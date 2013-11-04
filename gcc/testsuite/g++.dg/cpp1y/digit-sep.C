// { dg-options -std=c++1y }

#define assert(E) if(!(E))__builtin_abort();

#define m(x) 0

int
main()
{
  assert(1048576 == 1'048'576);
  assert(1048576 == 0X100000);
  assert(1048576 == 0x10'0000);
  assert(1048576 == 0'004'000'000);
  assert(1048576 == 0B100000000000000000000);
  assert(1048576 == 0b0001'0000'0000'0000'0000'0000);

  assert(1.602'176'565e-19 == 1.602176565e-19);
  assert(1.602'176'565e-1'9 == 1.602176565e-19);

  int i = m(1'2)+(3'4);
  assert(i == 34);
}
