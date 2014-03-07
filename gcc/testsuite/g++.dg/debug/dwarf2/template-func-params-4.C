// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR debug/30161
// { dg-do compile { target c++11 } }
// { dg-options "-g -dA" }
//
// In theory the compiler instantiates count<int, char, long>,
// count<char, long> and count<long>. In practice, only
// count<int, char, long> is emitted, thanks to constant folding.
// So in theory, each of the 3 instances of count yields a
// DW_TAG_GNU_template_parameter_pack DIE, but in practise, there is only one
// DW_TAG_GNU_template_parameter_pack as there is only count<int, char, long>
// is emitted.
// { dg-final { scan-assembler-times "DIE \\(0x\[^\n\]*\\) DW_TAG_GNU_template_parameter_pack" 1} }
// { dg-final { scan-assembler-times "DIE \\(0x\[^\n\]*\\) DW_TAG_template_type_param" 3} }


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

template<typename... P>
int
do_count()
{
  return count<P...>::value;
}

int c = do_count<int, char, long>();

