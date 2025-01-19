/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O2" } */

#include<cmath>
#include<cstddef>
#include<cstdint>
#include<iomanip>
#include<ios>
#include<iostream>
#include<memory>
#include<type_traits>
#include"riscv_vector.h"

using std::addressof;
using std::cerr;
using std::endl;
using std::int8_t;
using std::int16_t;
using std::int32_t;
using std::int64_t;
using std::uint8_t;
using std::uint16_t;
using std::uint32_t;
using std::uint64_t;
using std::ptrdiff_t;
using std::size_t;
using float16_t = _Float16;
using float32_t = float;
using float64_t = double;

template<class T, class T2>
constexpr T uint_to_float(T2 val) noexcept
{
  return *reinterpret_cast<T*>(&val);
}

constexpr const auto &f16(uint_to_float<float16_t, uint16_t>);
constexpr const auto &f32(uint_to_float<float32_t, uint32_t>);
constexpr const auto &f64(uint_to_float<float64_t, uint64_t>);

template<class T>
struct To_uint
{
  using type = std::conditional_t<
    sizeof(T) == 1, uint8_t, std::conditional_t<
      sizeof(T) == 2, uint16_t, std::conditional_t<
        sizeof(T) == 4, uint32_t, std::conditional_t<
          sizeof(T) == 8, uint64_t, void
        >
      >
    >
  >;
};

// isnan() does not support half type
template<class T>
struct To_float
{
  using type = std::conditional_t<
    std::is_same<T, float16_t>::value, float, std::conditional_t<
      std::is_same<T, float32_t>::value, float, std::conditional_t<
        std::is_same<T, float64_t>::value, double, float
      >
    >
  >;
};

template<class T>
using To_uint_t = typename To_uint<T>::type;

template<class T>
using To_isnan_float = typename To_float<T>::type;

template <class T>
void print_float(std::ostream &os, T val)
{
  using std::setw;
  os << std::hex << std::setfill('0') << setw(sizeof(T) * 2) << *reinterpret_cast<To_uint_t<T>*>(addressof(val)) << setw(0) << std::dec;
}

template <class T>
bool __attribute__((noinline))
check(const T *a, const T *b, size_t size)
{
  bool rv = true;
  for (size_t i = 0; i < (size / sizeof(T)); ++i) {
    if (reinterpret_cast<const To_uint_t<T> *>(a)[i] ==
        reinterpret_cast<const To_uint_t<T> *>(b)[i])
      continue;
    // floating negative zero == positive zero
    if ((std::is_floating_point_v<T> || std::is_same<T, float16_t>::value) &&
        (a[i] == b[i]))
      continue;
    // if both result are NaN, return true
    if ((std::is_same<T, float16_t>::value || std::is_floating_point_v<T>) &&
         std::isnan(static_cast<To_isnan_float<T>>(a[i])) &&
         std::isnan(static_cast<To_isnan_float<T>>(b[i])))
      continue;

    if (std::is_same<T, float16_t>::value) {
      cerr << std::hex << std::setfill('0') << std::setw(sizeof(T) * 2) << "["
           << i
           << "] result/golden:" << reinterpret_cast<const To_uint_t<T> *>(a)[i]
           << " != " << reinterpret_cast<const To_uint_t<T> *>(b)[i]
           << std::setw(0) << std::dec << endl;
    } else if constexpr (std::is_floating_point_v<T>) {
      cerr << "[" << i << "] result/golden:" << a[i] << "(";
      print_float(cerr, a[i]);
      cerr << ") != " << b[i] << "(";
      print_float(cerr, b[i]);
      cerr << ")" << endl;
    } else if constexpr (std::is_unsigned_v<T>) {
      cerr << "[" << i << "] result/golden: " << static_cast<uintmax_t>(a[i])
           << " != " << static_cast<uintmax_t>(b[i]) << endl;
    } else {
      cerr << "[" << i << "] result/golden:" << static_cast<intmax_t>(a[i])
           << " != " << static_cast<intmax_t>(b[i]) << endl;
    }
    rv = false;
  }
  return rv;
}

template <class T>
bool __attribute__((noinline))
check(const T a, const T golden)
{
  return check(addressof(a), addressof(golden), sizeof(T));
}

int main()
{
int return_value = 0;

size_t var_92 = 40u;
uint8_t var_91 [] = {11u, 89u, 54u, 232u, 165u, 85u, 209u, 49u, 34u, 129u, 156u, 155u, 32u, 161u, 238u, 63u, 5u, 117u, 182u, 0u, 211u, 168u, 96u, 29u, 14u, 126u, 10u, 182u, 32u, 49u, 205u, 40u, 242u, 167u, 114u, 76u, 146u, 122u, 141u, 124u};
size_t var_89 = 104u;
uint8_t var_88 [] = {62u, 152u, 160u, 146u, 231u, 221u, 197u, 169u, 123u, 6u, 58u, 112u, 253u, 208u, 191u, 20u, 252u, 94u, 153u, 134u, 34u, 255u, 70u, 192u, 92u, 194u, 227u, 42u, 117u, 86u, 177u, 130u, 23u, 123u, 59u, 3u, 8u, 96u, 34u, 117u, 209u, 89u, 177u, 105u, 171u, 73u, 84u, 177u, 66u, 8u, 171u, 91u, 59u, 32u, 206u, 23u, 12u, 94u, 48u, 208u, 252u, 160u, 140u, 169u, 245u, 186u, 118u, 128u, 19u, 12u, 35u, 20u, 243u, 67u, 0u, 121u, 205u, 1u, 53u, 39u, 22u, 21u, 220u, 108u, 83u, 21u, 142u, 61u, 231u, 27u, 106u, 100u, 46u, 237u, 80u, 25u, 238u, 182u, 142u, 11u, 139u, 102u, 221u, 145u};
size_t var_86 = 9u;
uint8_t var_85 [] = {41u, 156u, 138u, 248u, 254u, 36u, 60u, 13u, 62u};
size_t var_83 = 66u;
uint8_t var_82 [] = {157u, 65u, 122u, 81u, 72u, 231u, 126u, 145u, 164u, 9u, 174u, 111u, 94u, 210u, 246u, 229u, 223u, 225u, 88u, 154u, 115u, 219u, 171u, 61u, 142u, 174u, 253u, 88u, 155u, 102u, 56u, 242u, 55u, 123u, 104u, 208u, 245u, 160u, 143u, 62u, 227u, 215u, 67u, 198u, 195u, 103u, 203u, 252u, 90u, 186u, 194u, 240u, 182u, 80u, 38u, 139u, 70u, 110u, 220u, 56u, 138u, 128u, 201u, 119u, 253u, 189u};
size_t var_80 = 111u;
uint8_t var_79 [] = {157u, 65u, 122u, 81u, 72u, 231u, 126u, 145u, 164u, 9u, 174u, 111u, 94u, 210u, 246u, 229u, 223u, 225u, 88u, 154u, 115u, 219u, 171u, 61u, 142u, 174u, 253u, 88u, 155u, 102u, 56u, 242u, 55u, 123u, 104u, 208u, 245u, 160u, 143u, 62u, 227u, 215u, 67u, 198u, 195u, 103u, 203u, 252u, 90u, 186u, 194u, 240u, 182u, 80u, 38u, 139u, 70u, 110u, 220u, 56u, 138u, 128u, 201u, 119u, 253u, 189u, 149u, 254u, 229u, 77u, 79u, 142u, 64u, 49u, 235u, 129u, 207u, 223u, 156u, 3u, 23u, 90u, 144u, 151u, 179u, 40u, 62u, 22u, 245u, 208u, 56u, 147u, 38u, 65u, 72u, 211u, 148u, 144u, 226u, 8u, 228u, 238u, 79u, 121u, 164u, 199u, 29u, 221u, 228u, 205u, 42u};
size_t var_77 = 22u;
uint8_t var_76 [] = {157u, 65u, 122u, 81u, 72u, 231u, 126u, 145u, 164u, 9u, 174u, 111u, 94u, 210u, 246u, 229u, 223u, 225u, 88u, 154u, 115u, 219u};
size_t var_74 = 93u;
uint8_t var_73 [] = {160u, 130u, 232u, 162u, 72u, 128u, 128u, 34u, 64u, 18u, 128u, 128u, 128u, 72u, 128u, 160u, 128u, 194u, 88u, 104u, 152u, 216u, 88u, 160u, 128u, 128u, 160u, 88u, 216u, 128u, 56u, 200u, 128u, 216u, 104u, 208u, 160u, 160u, 128u, 128u, 24u, 128u, 24u, 128u, 24u, 128u, 88u, 192u, 104u, 232u, 8u, 240u, 128u, 80u, 128u, 88u, 128u, 128u, 192u, 56u, 40u, 128u, 146u, 128u, 160u, 160u, 160u, 128u, 160u, 160u, 128u, 128u, 64u, 98u, 88u, 2u, 128u, 128u, 192u, 24u, 128u, 104u, 144u, 128u, 152u, 40u, 128u, 128u, 160u, 208u, 56u, 152u, 128u};
size_t var_71 = 120u;
uint8_t var_70 [] = {160u, 130u, 232u, 162u, 72u, 128u, 128u, 34u, 64u, 18u, 128u, 128u, 128u, 72u, 128u, 160u, 128u, 194u, 88u, 104u, 152u, 216u, 88u, 160u, 128u, 128u, 160u, 88u, 216u, 128u, 56u, 200u, 128u, 216u, 104u, 208u, 160u, 160u, 128u, 128u, 24u, 128u, 24u, 128u, 24u, 128u, 88u, 192u, 104u, 232u, 8u, 240u, 128u, 80u, 128u, 88u, 128u, 128u, 192u, 56u, 40u, 128u, 146u, 128u, 160u, 160u, 160u, 128u, 160u, 160u, 128u, 128u, 64u, 98u, 88u, 2u, 128u, 128u, 192u, 24u, 128u, 104u, 144u, 128u, 152u, 40u, 128u, 128u, 160u, 208u, 56u, 152u, 128u, 130u, 72u, 152u, 64u, 144u, 136u, 8u, 64u, 128u, 128u, 242u, 64u, 128u, 160u, 160u, 64u, 160u, 168u, 192u, 192u, 128u, 128u, 168u, 128u, 232u, 128u, 192u};
size_t var_68 = 71u;
uint8_t var_67 [] = {160u, 130u, 232u, 162u, 72u, 128u, 128u, 34u, 64u, 18u, 128u, 128u, 128u, 72u, 128u, 160u, 128u, 194u, 88u, 104u, 152u, 216u, 88u, 160u, 128u, 128u, 160u, 88u, 216u, 128u, 56u, 200u, 128u, 216u, 104u, 208u, 160u, 160u, 128u, 128u, 24u, 128u, 24u, 128u, 24u, 128u, 88u, 192u, 104u, 232u, 8u, 240u, 128u, 80u, 128u, 88u, 128u, 128u, 192u, 56u, 40u, 128u, 146u, 128u, 160u, 160u, 160u, 128u, 160u, 160u, 128u};
size_t var_65 = 112u;
uint8_t var_64 [] = {157u, 65u, 122u, 81u, 72u, 231u, 126u, 145u, 164u, 9u, 174u, 111u, 94u, 210u, 246u, 229u, 223u, 225u, 88u, 154u, 115u, 219u, 171u, 61u, 142u, 174u, 253u, 88u, 155u, 102u, 56u, 242u, 55u, 123u, 104u, 208u, 245u, 160u, 143u, 62u, 227u, 215u, 67u, 198u, 195u, 103u, 203u, 252u, 90u, 186u, 194u, 240u, 182u, 80u, 38u, 139u, 70u, 110u, 220u, 56u, 138u, 128u, 201u, 119u, 253u, 189u, 149u, 254u, 229u, 77u, 79u, 142u, 64u, 49u, 235u, 129u, 207u, 223u, 156u, 3u, 23u, 90u, 144u, 151u, 179u, 40u, 62u, 22u, 245u, 208u, 56u, 147u, 38u, 65u, 72u, 211u, 148u, 144u, 226u, 8u, 228u, 238u, 79u, 121u, 164u, 199u, 29u, 221u, 228u, 205u, 42u, 252u};
size_t var_62 = 38u;
uint8_t var_61 [] = {160u, 130u, 232u, 162u, 72u, 128u, 128u, 34u, 64u, 18u, 128u, 128u, 128u, 72u, 128u, 160u, 128u, 194u, 88u, 104u, 152u, 216u, 88u, 160u, 128u, 128u, 160u, 88u, 216u, 128u, 56u, 200u, 128u, 216u, 104u, 208u, 160u, 160u};
size_t var_59 = 11u;
uint8_t var_58 [] = {160u, 130u, 232u, 162u, 72u, 128u, 128u, 34u, 64u, 18u, 128u};
size_t var_56 = 8u;
uint8_t var_55 [] = {160u, 130u, 232u, 162u, 72u, 128u, 128u, 34u};
size_t var_53 = 28u;
uint8_t var_52 [] = {0u, 144u, 64u, 144u, 64u, 0u, 0u, 144u, 0u, 16u, 0u, 0u, 0u, 64u, 0u, 0u, 0u, 144u, 192u, 64u, 192u, 192u, 192u, 0u, 0u, 0u, 0u, 192u};
size_t var_50 = 10u;
uint8_t var_49 [] = {0u, 144u, 64u, 144u, 64u, 0u, 0u, 144u, 0u, 16u};
uint8_t var_47 [] = {182u, 168u, 105u, 126u, 156u, 93u, 211u, 224u, 108u, 211u};
uint8_t var_46 [] = {45u, 125u, 169u, 242u, 191u, 145u, 8u, 31u, 96u, 206u, 73u, 124u, 253u, 19u, 130u, 21u, 151u, 145u, 236u, 250u, 205u, 187u, 99u, 161u, 79u, 225u, 245u, 167u};
uint8_t var_45 [] = {20u, 157u, 201u, 212u, 61u, 31u, 219u, 54u};
uint8_t var_44 [] = {144u, 25u, 221u, 0u, 167u, 170u, 8u, 210u, 158u, 111u, 234u};
uint8_t var_43 [] = {73u, 124u, 56u, 38u, 91u, 222u, 193u, 208u, 134u, 227u, 175u, 247u, 134u, 175u, 247u, 167u, 13u, 21u, 117u, 113u, 67u, 207u, 136u, 196u, 91u, 178u, 83u, 130u, 126u, 66u, 98u, 25u, 120u, 126u, 61u, 38u, 85u, 248u};
uint8_t var_42 [] = {166u, 143u, 183u, 66u, 11u, 148u, 1u, 53u, 150u, 98u, 7u, 112u, 72u, 240u, 124u, 254u, 158u, 123u, 75u, 76u, 168u, 55u, 94u, 73u, 0u, 171u, 205u, 242u, 140u, 100u, 81u, 238u, 187u, 7u, 154u, 144u, 30u, 226u, 0u, 10u, 137u, 96u, 145u, 198u, 230u, 148u, 232u, 198u, 13u, 31u, 134u, 229u, 253u, 112u, 46u, 52u, 195u, 168u, 76u, 147u, 153u, 95u, 32u, 115u, 79u, 214u, 80u, 202u, 90u, 7u, 156u, 39u, 185u, 143u, 79u, 247u, 27u, 103u, 20u, 180u, 0u, 127u, 110u, 185u, 195u, 95u, 200u, 104u, 150u, 246u, 58u, 33u, 159u, 94u, 95u, 51u, 3u, 43u, 153u, 244u, 114u, 127u, 244u, 134u, 98u, 151u, 77u, 219u, 219u, 229u, 194u, 119u};
uint8_t var_41 [] = {31u, 75u, 127u, 254u, 94u, 180u, 217u, 67u, 53u, 241u, 253u, 128u, 200u, 4u, 41u, 179u, 169u, 9u, 230u, 18u, 55u, 240u, 178u, 135u, 193u, 231u, 106u, 167u, 145u, 109u, 52u, 151u, 19u, 133u, 176u, 22u, 214u, 120u, 29u, 131u, 207u, 31u, 33u, 237u, 190u, 16u, 33u, 107u, 126u, 114u, 63u, 107u, 158u, 202u, 172u, 203u, 60u, 202u, 179u, 11u, 226u, 186u, 230u, 163u, 191u, 127u, 251u, 192u, 76u, 235u, 58u};
uint8_t var_40 [] = {145u, 253u, 8u, 228u, 212u, 153u, 22u, 179u, 125u, 54u, 30u, 83u, 110u, 236u, 137u, 211u, 226u, 153u, 137u, 115u, 124u, 6u, 53u, 57u, 97u, 35u, 0u, 87u, 113u, 115u, 229u, 133u, 220u, 24u, 224u, 134u, 146u, 89u, 206u, 243u, 59u, 36u, 21u, 4u, 145u, 1u, 55u, 48u, 102u, 228u, 96u, 95u, 11u, 196u, 142u, 92u, 182u, 250u, 113u, 210u, 36u, 132u, 122u, 241u, 182u, 251u, 22u, 98u, 225u, 186u, 8u, 36u, 242u, 83u, 43u, 136u, 123u, 214u, 248u, 176u, 247u, 62u, 235u, 88u, 78u, 42u, 29u, 46u, 37u, 209u, 215u, 146u, 112u, 130u, 230u, 170u, 60u, 94u, 204u, 21u, 247u, 209u, 47u, 96u, 55u, 128u, 129u, 236u, 47u, 39u, 29u, 17u, 152u, 58u, 179u, 170u, 219u, 61u, 5u, 134u};
uint8_t var_39 [] = {88u, 39u, 186u, 135u, 230u, 157u, 86u, 109u, 133u, 40u, 183u, 209u, 6u, 228u, 76u, 244u, 54u, 141u, 116u, 46u, 52u, 189u, 83u, 150u, 24u, 240u, 4u, 145u, 57u, 95u, 47u, 251u, 230u, 31u, 206u, 87u, 126u, 6u, 137u, 195u, 21u, 209u, 219u, 116u, 42u, 58u, 245u, 211u, 120u, 15u, 23u, 132u, 155u, 166u, 126u, 203u, 32u, 177u, 137u, 160u, 141u, 192u, 1u, 27u, 222u, 63u, 242u, 163u, 34u, 133u, 12u, 149u, 220u, 1u, 5u, 79u, 114u, 174u, 116u, 60u, 42u, 170u, 56u, 101u, 119u, 231u, 181u, 126u, 133u, 122u, 58u, 62u, 171u};
uint8_t var_38 [] = {91u, 29u, 1u, 213u, 109u, 30u, 128u, 157u, 89u, 130u, 248u, 205u, 230u, 36u, 213u, 8u, 31u, 203u, 39u, 226u, 71u, 117u};
uint8_t var_37 [] = {186u, 118u, 5u, 54u, 206u, 3u, 160u, 246u, 239u, 211u, 137u, 182u, 119u, 103u, 85u, 103u, 158u, 81u, 121u, 131u, 241u, 153u, 71u, 8u, 101u, 194u, 212u, 225u, 132u, 59u, 130u, 13u, 124u, 233u, 242u, 172u, 121u, 201u, 148u, 86u, 59u, 185u, 188u, 230u, 85u, 200u, 193u, 212u, 2u, 33u, 98u, 115u, 101u, 12u, 95u, 73u, 165u, 64u, 255u, 81u, 178u, 189u, 45u, 147u, 88u, 84u, 119u, 119u, 180u, 59u, 234u, 50u, 90u, 69u, 15u, 193u, 36u, 142u, 32u, 71u, 157u, 80u, 190u, 196u, 117u, 201u, 117u, 244u, 136u, 30u, 76u, 223u, 115u, 217u, 233u, 71u, 126u, 61u, 180u, 141u, 31u, 138u, 164u, 73u, 77u, 67u, 75u, 49u, 9u, 117u, 193u};
uint8_t var_36 [] = {83u, 227u, 251u, 221u, 121u, 11u, 108u, 49u, 182u, 185u, 143u, 124u, 153u, 207u, 151u, 217u, 95u, 249u, 21u, 118u, 161u, 231u, 47u, 253u, 218u, 94u, 209u, 113u, 131u, 50u, 68u, 83u, 44u, 213u, 8u, 27u, 28u, 16u, 65u, 19u, 89u, 37u, 103u, 134u, 7u, 80u, 28u, 54u, 18u, 162u, 179u, 203u, 1u, 87u, 141u, 43u, 183u, 2u, 147u, 175u, 98u, 120u, 120u, 239u, 207u, 154u};
uint8_t var_35 [] = {79u, 217u, 102u, 16u, 233u, 36u, 243u, 16u, 97u};
uint8_t var_34 [] = {249u, 99u, 140u, 174u, 222u, 81u, 130u, 62u, 196u, 126u, 217u, 53u, 49u, 151u, 240u, 108u, 235u, 182u, 49u, 59u, 206u, 230u, 35u, 97u, 191u, 228u, 219u, 76u, 116u, 4u, 76u, 111u, 19u, 189u, 13u, 154u, 110u, 52u, 207u, 245u, 82u, 13u, 67u, 11u, 18u, 8u, 223u, 87u, 69u, 226u, 65u, 128u, 195u, 233u, 189u, 47u, 83u, 45u, 97u, 117u, 137u, 163u, 174u, 213u, 235u, 249u, 199u, 64u, 158u, 197u, 100u, 244u, 84u, 203u, 141u, 72u, 174u, 162u, 65u, 44u, 62u, 214u, 67u, 21u, 126u, 243u, 141u, 12u, 213u, 54u, 114u, 166u, 209u, 28u, 88u, 187u, 227u, 33u, 119u, 83u, 70u, 239u, 223u, 70u};
uint8_t var_33 [] = {69u, 55u, 127u, 240u, 221u, 161u, 186u, 218u, 11u, 54u, 10u, 236u, 49u, 42u, 221u, 26u, 11u, 151u, 177u, 93u, 119u, 141u, 77u, 140u, 187u, 222u, 36u, 255u, 206u, 131u, 24u, 15u, 135u, 189u, 186u, 178u, 168u, 159u, 70u, 9u};
uint8_t var_32 [] = {11u, 89u, 54u, 232u, 165u, 85u, 209u, 49u, 34u, 129u, 156u, 155u, 32u, 161u, 238u, 63u, 5u, 117u, 182u, 0u, 211u, 168u, 96u, 29u, 14u, 126u, 10u, 182u, 32u, 49u, 205u, 40u, 242u, 167u, 114u, 76u, 146u, 122u, 141u, 124u};
uint8_t var_31 [] = {62u, 152u, 160u, 146u, 231u, 221u, 197u, 169u, 123u, 6u, 58u, 112u, 253u, 208u, 191u, 20u, 252u, 94u, 153u, 134u, 34u, 255u, 70u, 192u, 92u, 194u, 227u, 42u, 117u, 86u, 177u, 130u, 23u, 123u, 59u, 3u, 8u, 96u, 34u, 117u, 209u, 89u, 177u, 105u, 171u, 73u, 84u, 177u, 66u, 8u, 171u, 91u, 59u, 32u, 206u, 23u, 12u, 94u, 48u, 208u, 252u, 160u, 140u, 169u, 245u, 186u, 118u, 128u, 19u, 12u, 35u, 20u, 243u, 67u, 0u, 121u, 205u, 1u, 53u, 39u, 22u, 21u, 220u, 108u, 83u, 21u, 142u, 61u, 231u, 27u, 106u, 100u, 46u, 237u, 80u, 25u, 238u, 182u, 142u, 11u, 139u, 102u, 221u, 145u};
uint8_t var_30 [] = {41u, 156u, 138u, 248u, 254u, 36u, 60u, 13u, 62u};
uint8_t var_29 [] = {157u, 65u, 122u, 81u, 72u, 231u, 126u, 145u, 164u, 9u, 174u, 111u, 94u, 210u, 246u, 229u, 223u, 225u, 88u, 154u, 115u, 219u, 171u, 61u, 142u, 174u, 253u, 88u, 107u, 19u, 106u, 124u, 175u, 16u, 255u, 243u, 144u, 185u, 238u, 120u, 134u, 137u, 173u, 235u, 151u, 203u, 186u, 150u, 72u, 231u, 96u, 210u, 237u, 52u, 133u, 105u, 252u, 158u, 43u, 9u, 203u, 255u, 200u, 19u, 185u, 30u, 205u, 15u, 112u, 214u, 13u, 106u, 178u, 19u, 164u, 242u, 205u, 109u, 158u, 205u, 241u, 172u, 195u, 140u, 200u, 101u, 242u, 90u, 96u, 231u, 211u, 143u, 54u, 210u, 175u, 104u, 38u, 3u, 242u, 10u, 245u, 118u, 65u, 95u, 193u, 2u, 55u, 89u, 10u, 145u, 91u, 128u, 59u, 24u, 242u, 132u, 202u, 141u, 129u, 164u};
uint8_t var_28 [] = {155u, 102u, 56u, 242u, 55u, 123u, 104u, 208u, 245u, 160u, 143u, 62u, 227u, 215u, 67u, 198u, 195u, 103u, 203u, 252u, 90u, 186u, 194u, 240u, 182u, 80u, 38u, 139u, 70u, 110u, 220u, 56u, 138u, 128u, 201u, 119u, 253u, 189u, 149u, 254u, 229u, 77u, 79u, 142u, 64u, 49u, 235u, 129u, 207u, 223u, 156u, 3u, 23u, 90u, 144u, 151u, 179u, 40u, 62u, 22u, 245u, 208u, 56u, 147u, 38u, 65u, 72u, 211u, 148u, 144u, 226u, 8u, 228u, 238u, 79u, 121u, 164u, 199u, 29u, 221u, 228u, 205u, 42u, 252u, 236u, 63u, 128u, 106u, 30u, 122u, 30u, 44u, 9u, 129u, 53u, 11u, 59u, 60u, 117u, 31u, 111u, 219u, 158u, 14u, 148u, 223u, 228u, 160u, 237u, 78u, 122u, 253u, 205u, 34u, 252u, 39u, 147u, 211u, 17u, 63u};
uint16_t var_27 [] = {20665u, 64011u, 44313u, 18011u, 56759u, 41131u, 2113u, 20357u, 4366u, 41922u, 13219u, 29124u, 53134u, 63195u, 41618u, 17798u, 30352u, 26110u, 32335u, 3191u, 59414u, 16846u, 40269u, 16422u, 55562u, 9133u, 645u, 40218u};
size_t var_26 = 28u;
__riscv_vsetvl_e8m8(40);
vuint8m8_t var_20 = __riscv_vle8_v_u8m8(var_32, 40);
// 11, 89, 54, 232, 165, 85, 209, 49, 34, 129, 156, 155, 32, 161, 238, 63, 5, 117, 182, 0, 211, 168, 96, 29, 14, 126, 10, 182, 32, 49, 205, 40, 242, 167, 114, 76, 146, 122, 141, 124

__riscv_vsetvl_e8m8(104);
vuint8m8_t var_21 = __riscv_vle8_v_u8m8(var_31, 104);
// 62, 152, 160, 146, 231, 221, 197, 169, 123, 6, 58, 112, 253, 208, 191, 20, 252, 94, 153, 134, 34, 255, 70, 192, 92, 194, 227, 42, 117, 86, 177, 130, 23, 123, 59, 3, 8, 96, 34, 117, 209, 89, 177, 105, 171, 73, 84, 177, 66, 8, 171, 91, 59, 32, 206, 23, 12, 94, 48, 208, 252, 160, 140, 169, 245, 186, 118, 128, 19, 12, 35, 20, 243, 67, 0, 121, 205, 1, 53, 39, 22, 21, 220, 108, 83, 21, 142, 61, 231, 27, 106, 100, 46, 237, 80, 25, 238, 182, 142, 11, 139, 102, 221, 145

__riscv_vsetvl_e8m8(9);
vuint8m8_t var_22 = __riscv_vle8_v_u8m8(var_30, 9);
// 41, 156, 138, 248, 254, 36, 60, 13, 62

__riscv_vsetvl_e8m8(120);
vuint8m8_t var_23 = __riscv_vle8_v_u8m8(var_29, 120);
// 157, 65, 122, 81, 72, 231, 126, 145, 164, 9, 174, 111, 94, 210, 246, 229, 223, 225, 88, 154, 115, 219, 171, 61, 142, 174, 253, 88, 107, 19, 106, 124, 175, 16, 255, 243, 144, 185, 238, 120, 134, 137, 173, 235, 151, 203, 186, 150, 72, 231, 96, 210, 237, 52, 133, 105, 252, 158, 43, 9, 203, 255, 200, 19, 185, 30, 205, 15, 112, 214, 13, 106, 178, 19, 164, 242, 205, 109, 158, 205, 241, 172, 195, 140, 200, 101, 242, 90, 96, 231, 211, 143, 54, 210, 175, 104, 38, 3, 242, 10, 245, 118, 65, 95, 193, 2, 55, 89, 10, 145, 91, 128, 59, 24, 242, 132, 202, 141, 129, 164

vuint8m8_t var_24 = __riscv_vle8_v_u8m8(var_28, 120);
// 155, 102, 56, 242, 55, 123, 104, 208, 245, 160, 143, 62, 227, 215, 67, 198, 195, 103, 203, 252, 90, 186, 194, 240, 182, 80, 38, 139, 70, 110, 220, 56, 138, 128, 201, 119, 253, 189, 149, 254, 229, 77, 79, 142, 64, 49, 235, 129, 207, 223, 156, 3, 23, 90, 144, 151, 179, 40, 62, 22, 245, 208, 56, 147, 38, 65, 72, 211, 148, 144, 226, 8, 228, 238, 79, 121, 164, 199, 29, 221, 228, 205, 42, 252, 236, 63, 128, 106, 30, 122, 30, 44, 9, 129, 53, 11, 59, 60, 117, 31, 111, 219, 158, 14, 148, 223, 228, 160, 237, 78, 122, 253, 205, 34, 252, 39, 147, 211, 17, 63

__riscv_vsetvl_e16m4(28);
vuint16m4_t var_25 = __riscv_vle16_v_u16m4(var_27, 28);
// 20665, 64011, 44313, 18011, 56759, 41131, 2113, 20357, 4366, 41922, 13219, 29124, 53134, 63195, 41618, 17798, 30352, 26110, 32335, 3191, 59414, 16846, 40269, 16422, 55562, 9133, 645, 40218

__riscv_vsetvl_e8m8(40);
__riscv_vse8_v_u8m8(var_33, var_20, 40);
__riscv_vsetvl_e8m8(104);
__riscv_vse8_v_u8m8(var_34, var_21, 104);
__riscv_vsetvl_e8m8(9);
__riscv_vse8_v_u8m8(var_35, var_22, 9);
__riscv_vsetvl_e8m8(120);
vuint8m8_t var_16 = __riscv_vslideup_vx_u8m8(var_23, var_24, var_26, 120);
// 157, 65, 122, 81, 72, 231, 126, 145, 164, 9, 174, 111, 94, 210, 246, 229, 223, 225, 88, 154, 115, 219, 171, 61, 142, 174, 253, 88, 155, 102, 56, 242, 55, 123, 104, 208, 245, 160, 143, 62, 227, 215, 67, 198, 195, 103, 203, 252, 90, 186, 194, 240, 182, 80, 38, 139, 70, 110, 220, 56, 138, 128, 201, 119, 253, 189, 149, 254, 229, 77, 79, 142, 64, 49, 235, 129, 207, 223, 156, 3, 23, 90, 144, 151, 179, 40, 62, 22, 245, 208, 56, 147, 38, 65, 72, 211, 148, 144, 226, 8, 228, 238, 79, 121, 164, 199, 29, 221, 228, 205, 42, 252, 236, 63, 128, 106, 30, 122, 30, 44

__riscv_vsetvl_e8m2(28);
vuint8m2_t var_8 = __riscv_vncvt_x_x_w_u8m2(var_25, 28);
// 185, 11, 25, 91, 183, 171, 65, 133, 14, 194, 163, 196, 142, 219, 146, 134, 144, 254, 79, 119, 22, 206, 77, 38, 10, 173, 133, 26

if(!check(var_33, var_91, var_92)) {cerr << "check 90 fails" << endl; return_value = 1;}
if(!check(var_34, var_88, var_89)) {cerr << "check 87 fails" << endl; return_value = 1;}
if(!check(var_35, var_85, var_86)) {cerr << "check 84 fails" << endl; return_value = 1;}
__riscv_vsetvl_e8m8(111);
__riscv_vse8_v_u8m8(var_37, var_16, 111);
__riscv_vsetvl_e8m8(112);
__riscv_vse8_v_u8m8(var_42, var_16, 112);
__riscv_vsetvl_e8m8(66);
__riscv_vse8_v_u8m8(var_36, var_16, 66);
__riscv_vsetvl_e8m8(22);
__riscv_vse8_v_u8m8(var_38, var_16, 22);
__riscv_vsetvl_e8m8(120);
vuint8m8_t var_13 = __riscv_vsll_vv_u8m8(var_16, var_16, 120);
// 160, 130, 232, 162, 72, 128, 128, 34, 64, 18, 128, 128, 128, 72, 128, 160, 128, 194, 88, 104, 152, 216, 88, 160, 128, 128, 160, 88, 216, 128, 56, 200, 128, 216, 104, 208, 160, 160, 128, 128, 24, 128, 24, 128, 24, 128, 88, 192, 104, 232, 8, 240, 128, 80, 128, 88, 128, 128, 192, 56, 40, 128, 146, 128, 160, 160, 160, 128, 160, 160, 128, 128, 64, 98, 88, 2, 128, 128, 192, 24, 128, 104, 144, 128, 152, 40, 128, 128, 160, 208, 56, 152, 128, 130, 72, 152, 64, 144, 136, 8, 64, 128, 128, 242, 64, 128, 160, 160, 64, 160, 168, 192, 192, 128, 128, 168, 128, 232, 128, 192

__riscv_vsetvl_e8m2(28);
uint8_t var_4 = __riscv_vmv_x_s_u8m2_u8(var_8);
// 185

if(!check(var_37, var_79, var_80)) {cerr << "check 78 fails" << endl; return_value = 1;}
if(!check(var_42, var_64, var_65)) {cerr << "check 63 fails" << endl; return_value = 1;}
if(!check(var_36, var_82, var_83)) {cerr << "check 81 fails" << endl; return_value = 1;}
if(!check(var_38, var_76, var_77)) {cerr << "check 75 fails" << endl; return_value = 1;}
__riscv_vsetvl_e8m8(8);
__riscv_vse8_v_u8m8(var_45, var_13, 8);
__riscv_vsetvl_e8m8(38);
__riscv_vse8_v_u8m8(var_43, var_13, 38);
__riscv_vsetvl_e8m8(120);
__riscv_vse8_v_u8m8(var_40, var_13, 120);
__riscv_vsetvl_e8m8(71);
__riscv_vse8_v_u8m8(var_41, var_13, 71);
__riscv_vsetvl_e8m8(11);
__riscv_vse8_v_u8m8(var_44, var_13, 11);
__riscv_vsetvl_e8m8(93);
__riscv_vse8_v_u8m8(var_39, var_13, 93);
__riscv_vsetvl_e8m8(28);
vuint8m8_t var_2 = __riscv_vnmsub_vx_u8m8(var_13, var_4, var_13, 28);
// 0, 144, 64, 144, 64, 0, 0, 144, 0, 16, 0, 0, 0, 64, 0, 0, 0, 144, 192, 64, 192, 192, 192, 0, 0, 0, 0, 192, 216, 128, 56, 200, 128, 216, 104, 208, 160, 160, 128, 128, 24, 128, 24, 128, 24, 128, 88, 192, 104, 232, 8, 240, 128, 80, 128, 88, 128, 128, 192, 56, 40, 128, 146, 128, 160, 160, 160, 128, 160, 160, 128, 128, 64, 98, 88, 2, 128, 128, 192, 24, 128, 104, 144, 128, 152, 40, 128, 128, 160, 208, 56, 152, 128, 130, 72, 152, 64, 144, 136, 8, 64, 128, 128, 242, 64, 128, 160, 160, 64, 160, 168, 192, 192, 128, 128, 168, 128, 232, 128, 192

if(!check(var_45, var_55, var_56)) {cerr << "check 54 fails" << endl; return_value = 1;}
if(!check(var_43, var_61, var_62)) {cerr << "check 60 fails" << endl; return_value = 1;}
if(!check(var_40, var_70, var_71)) {cerr << "check 69 fails" << endl; return_value = 1;}
if(!check(var_41, var_67, var_68)) {cerr << "check 66 fails" << endl; return_value = 1;}
if(!check(var_44, var_58, var_59)) {cerr << "check 57 fails" << endl; return_value = 1;}
if(!check(var_39, var_73, var_74)) {cerr << "check 72 fails" << endl; return_value = 1;}
__riscv_vse8_v_u8m8(var_46, var_2, 28);
__riscv_vsetvl_e8m8(10);
__riscv_vse8_v_u8m8(var_47, var_2, 10);
if(!check(var_46, var_52, var_53)) {cerr << "check 51 fails" << endl; return_value = 1;}
if(!check(var_47, var_49, var_50)) {cerr << "check 48 fails" << endl; return_value = 1;}
if (return_value)
  __builtin_abort ();
return return_value;
}
