// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/30161
// { dg-options "-std=c++0x -g -dA" }
// { dg-final { scan-assembler "DW_TAG_template_type_param" } }
// { dg-final { scan-assembler "DW_AT_name.*Args#0" } }
// { dg-final { scan-assembler "DW_AT_name.*Args#1" } }
// { dg-final { scan-assembler "DW_AT_name.*Args#2" } }

template <typename... Args> struct count;

template <>
struct count<>
{
  static const int value = 0;
};

template <typename T, typename... Args>
struct count<T, Args...>
{
  static const int value = 1 + count<Args...>::value;
};

int
foo ()
{
  count<int, char, long> c;
  int nb = count<int, char, long>::value;
  return nb;
}

