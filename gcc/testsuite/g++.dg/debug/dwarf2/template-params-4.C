// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/30161
// { dg-options "-std=c++0x -g -dA" }
//
// In theory the compiler instantiates count<int, char, long>,
// count<char, long> and count<long>. In practice, only
// count<int, char, long> is emitted, thanks to constant folding.
// So in theory, each of the 3 instances of count yields a
// DW_TAG_tempalate_parameter_pack DIE, but in practise, there is only one
// DW_TAG_template_parameter_pack as there is only count<int, char, long>
// is emitted.
// { dg-final { scan-assembler-times "DIE \\(0x.*?\\) DW_TAG_template_parameter_pack" 1} }
// { dg-final { scan-assembler-times "DIE \\(0x.*?\\) DW_TAG_template_type_param" 3} }

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

