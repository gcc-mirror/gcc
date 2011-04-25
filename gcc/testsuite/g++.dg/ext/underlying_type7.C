// { dg-do compile }
// { dg-options "-std=c++0x" }

enum E1 : unsigned { E1_en = 1 };
enum E2 : char { E2_en = 1 };
enum class E3 { a = -1 };
enum class E4 : unsigned char { c = 1 };
enum class E5 : int { a = -1, b = 1 };
enum class E6 : long { c = __LONG_MAX__ };

template<typename T>
  void
  test(T, __underlying_type(T)) // { dg-message "sorry, unimplemented: mangling" }
  { }

int main()
{
  test(E1::E1_en, 1);
  test(E2::E2_en, 1);
  test(E3::a, -1);
  test(E4::c, 1);
  test(E5::a, -1);
  test(E6::c, __LONG_MAX__);
}
