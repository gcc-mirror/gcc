/* { dg-do run { target { riscv_vector } } } */
/* { dg-options "-O2" } */

#include<cstdalign>
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

size_t var_192 = 4u;
uint8_t var_191 [] = {102u, 37u, 103u, 76u};
size_t var_189 = 28u;
uint8_t var_188 [] = {218u, 150u, 128u, 18u, 225u, 117u, 251u, 84u, 180u, 34u, 174u, 111u, 22u, 157u, 36u, 97u, 83u, 101u, 93u, 143u, 249u, 110u, 224u, 213u, 111u, 205u, 125u, 70u};
size_t var_186 = 15u;
uint8_t var_185 [] = {43u, 165u, 118u, 143u, 24u, 178u, 108u, 42u, 168u, 165u, 210u, 11u, 242u, 151u, 76u};
size_t var_183 = 9u;
uint8_t var_182 [] = {124u, 139u, 130u, 178u, 131u, 94u, 153u, 139u, 0u};
size_t var_180 = 5u;
uint8_t var_179 [] = {163u, 112u, 39u, 165u, 124u};
size_t var_177 = 13u;
uint8_t var_176 [] = {8u, 244u, 149u, 106u, 11u, 74u, 10u, 105u, 180u, 241u, 53u, 11u, 14u};
size_t var_174 = 10u;
uint8_t var_173 [] = {7u, 86u, 55u, 184u, 15u, 158u, 72u, 233u, 133u, 233u};
size_t var_171 = 20u;
uint8_t var_170 [] = {51u, 84u, 251u, 79u, 169u, 178u, 97u, 33u, 90u, 0u, 91u, 158u, 224u, 205u, 75u, 28u, 3u, 218u, 110u, 90u};
size_t var_168 = 64u;
uint8_t var_167 [] = {193u, 150u, 153u, 49u, 201u, 26u, 151u, 177u, 167u, 78u, 105u, 182u, 74u, 188u, 91u, 167u, 169u, 137u, 194u, 116u, 89u, 32u, 229u, 69u, 41u, 252u, 20u, 101u, 127u, 181u, 204u, 81u, 157u, 27u, 85u, 143u, 67u, 82u, 45u, 110u, 157u, 132u, 218u, 206u, 0u, 101u, 226u, 175u, 46u, 227u, 57u, 15u, 219u, 103u, 165u, 35u, 243u, 191u, 90u, 217u, 152u, 19u, 91u, 133u};
size_t var_165 = 64u;
uint8_t var_164 [] = {77u, 114u, 183u, 24u, 17u, 94u, 48u, 21u, 117u, 158u, 135u, 138u, 31u, 122u, 24u, 134u, 116u, 146u, 130u, 126u, 60u, 82u, 62u, 217u, 175u, 210u, 31u, 180u, 137u, 136u, 119u, 224u, 115u, 60u, 73u, 187u, 165u, 6u, 123u, 41u, 140u, 75u, 173u, 237u, 210u, 128u, 157u, 81u, 173u, 227u, 236u, 176u, 203u, 81u, 113u, 211u, 142u, 120u, 71u, 3u, 215u, 194u, 147u, 7u};
size_t var_162 = 64u;
uint8_t var_161 [] = {153u, 5u, 190u, 157u, 3u, 204u, 9u, 23u, 106u, 85u, 136u, 151u, 36u, 117u, 108u, 208u, 112u, 101u, 37u, 170u, 107u, 153u, 81u, 21u, 239u, 77u, 211u, 159u, 1u, 231u, 118u, 233u, 97u, 229u, 24u, 35u, 33u, 113u, 169u, 198u, 192u, 22u, 103u, 8u, 176u, 225u, 132u, 22u, 195u, 77u, 229u, 177u, 38u, 149u, 49u, 70u, 33u, 139u, 198u, 128u, 246u, 221u, 105u, 211u};
size_t var_159 = 64u;
uint8_t var_158 [] = {188u, 58u, 189u, 90u, 72u, 84u, 231u, 86u, 207u, 205u, 250u, 157u, 37u, 30u, 193u, 252u, 149u, 65u, 253u, 57u, 125u, 26u, 29u, 236u, 55u, 174u, 88u, 53u, 236u, 118u, 204u, 66u, 70u, 228u, 8u, 74u, 97u, 162u, 220u, 206u, 143u, 50u, 193u, 29u, 117u, 127u, 236u, 135u, 143u, 224u, 30u, 170u, 236u, 210u, 118u, 122u, 13u, 193u, 250u, 77u, 162u, 235u, 191u, 123u};
size_t var_156 = 64u;
uint8_t var_155 [] = {159u, 134u, 70u, 200u, 59u, 48u, 113u, 117u, 231u, 150u, 13u, 183u, 113u, 25u, 160u, 112u, 25u, 5u, 193u, 228u, 218u, 71u, 75u, 70u, 229u, 34u, 169u, 0u, 153u, 225u, 29u, 188u, 91u, 124u, 51u, 133u, 74u, 135u, 145u, 161u, 5u, 16u, 159u, 184u, 207u, 228u, 146u, 219u, 135u, 122u, 70u, 63u, 53u, 20u, 137u, 143u, 109u, 121u, 26u, 29u, 158u, 80u, 50u, 221u};
size_t var_153 = 64u;
uint8_t var_152 [] = {79u, 159u, 83u, 78u, 250u, 26u, 87u, 16u, 25u, 209u, 33u, 16u, 225u, 96u, 23u, 234u, 142u, 195u, 225u, 96u, 171u, 47u, 189u, 12u, 113u, 206u, 40u, 71u, 78u, 21u, 238u, 249u, 180u, 79u, 69u, 30u, 155u, 99u, 186u, 232u, 222u, 44u, 86u, 201u, 140u, 83u, 211u, 90u, 187u, 95u, 213u, 55u, 87u, 13u, 234u, 182u, 142u, 218u, 205u, 113u, 119u, 200u, 212u, 212u};
size_t var_150 = 64u;
uint8_t var_149 [] = {228u, 198u, 218u, 214u, 109u, 137u, 15u, 175u, 108u, 239u, 230u, 253u, 181u, 238u, 239u, 198u, 214u, 161u, 167u, 255u, 83u, 102u, 254u, 197u, 197u, 186u, 46u, 112u, 173u, 103u, 78u, 76u, 239u, 125u, 111u, 102u, 146u, 94u, 237u, 250u, 191u, 150u, 91u, 143u, 133u, 251u, 89u, 153u, 181u, 197u, 45u, 242u, 4u, 179u, 51u, 155u, 2u, 145u, 194u, 183u, 117u, 46u, 237u, 93u};
size_t var_147 = 64u;
uint8_t var_146 [] = {119u, 148u, 1u, 75u, 38u, 246u, 148u, 30u, 43u, 208u, 250u, 228u, 52u, 216u, 11u, 216u, 223u, 55u, 17u, 110u, 248u, 126u, 249u, 164u, 77u, 108u, 220u, 253u, 123u, 61u, 197u, 169u, 231u, 139u, 235u, 206u, 53u, 29u, 198u, 94u, 172u, 30u, 206u, 241u, 124u, 19u, 214u, 130u, 190u, 183u, 42u, 225u, 164u, 84u, 129u, 198u, 240u, 113u, 22u, 22u, 88u, 242u, 188u, 106u};
size_t var_144 = 64u;
uint8_t var_143 [] = {223u, 132u, 231u, 84u, 77u, 212u, 175u, 94u, 84u, 109u, 23u, 55u, 39u, 41u, 138u, 83u, 41u, 79u, 40u, 181u, 204u, 178u, 59u, 224u, 76u, 207u, 64u, 199u, 157u, 29u, 235u, 90u, 171u, 129u, 222u, 86u, 72u, 97u, 94u, 74u, 183u, 187u, 147u, 10u, 215u, 89u, 234u, 60u, 243u, 180u, 105u, 49u, 197u, 207u, 35u, 251u, 153u, 111u, 195u, 122u, 211u, 197u, 35u, 74u};
size_t var_141 = 64u;
uint8_t var_140 [] = {101u, 3u, 188u, 105u, 18u, 217u, 42u, 77u, 31u, 217u, 220u, 77u, 84u, 145u, 131u, 31u, 211u, 237u, 57u, 88u, 203u, 161u, 221u, 237u, 253u, 18u, 85u, 120u, 182u, 16u, 143u, 147u, 54u, 234u, 91u, 19u, 101u, 182u, 231u, 211u, 226u, 112u, 5u, 36u, 177u, 202u, 102u, 115u, 123u, 101u, 180u, 218u, 175u, 219u, 61u, 77u, 122u, 29u, 43u, 30u, 214u, 108u, 204u, 157u};
size_t var_138 = 64u;
uint8_t var_137 [] = {42u, 93u, 89u, 6u, 100u, 143u, 38u, 7u, 46u, 15u, 31u, 241u, 23u, 37u, 103u, 102u, 23u, 138u, 45u, 239u, 76u, 191u, 112u, 167u, 90u, 200u, 40u, 61u, 137u, 51u, 38u, 144u, 151u, 197u, 152u, 43u, 80u, 162u, 113u, 99u, 93u, 63u, 32u, 146u, 129u, 195u, 175u, 127u, 204u, 223u, 99u, 217u, 247u, 60u, 161u, 231u, 136u, 51u, 101u, 97u, 13u, 247u, 226u, 198u};
size_t var_135 = 64u;
uint8_t var_134 [] = {142u, 79u, 103u, 76u, 102u, 37u, 29u, 203u, 48u, 40u, 10u, 81u, 10u, 247u, 251u, 199u, 10u, 92u, 66u, 83u, 212u, 79u, 17u, 102u, 211u, 60u, 136u, 183u, 253u, 50u, 39u, 207u, 115u, 9u, 34u, 28u, 153u, 200u, 20u, 149u, 63u, 212u, 209u, 34u, 34u, 176u, 163u, 50u, 2u, 170u, 121u, 59u, 58u, 157u, 73u, 97u, 174u, 63u, 88u, 69u, 147u, 37u, 229u, 166u};
uint8_t var_132 [] = {45u, 14u, 147u, 190u};
uint8_t var_131 [] = {142u, 79u, 184u, 76u, 148u, 29u, 29u, 203u, 48u, 40u, 10u, 81u, 10u, 247u, 251u, 199u, 10u, 92u, 66u, 83u, 212u, 79u, 17u, 102u, 211u, 60u, 136u, 183u, 253u, 50u, 39u, 207u, 115u, 9u, 34u, 28u, 153u, 200u, 20u, 149u, 63u, 212u, 209u, 34u, 34u, 176u, 163u, 50u, 2u, 170u, 121u, 59u, 58u, 157u, 73u, 97u, 174u, 63u, 88u, 69u, 147u, 37u, 229u, 166u};
uint8_t var_129 [] = {4u, 5u, 2u, 3u};
uint8_t var_127 [] = {42u, 143u, 89u, 3u, 151u, 51u, 254u, 7u, 76u, 15u, 23u, 241u, 23u, 236u, 129u, 92u, 23u, 138u, 45u, 255u, 87u, 191u, 112u, 167u, 90u, 200u, 40u, 61u, 137u, 51u, 38u, 144u, 151u, 197u, 152u, 43u, 80u, 162u, 113u, 99u, 93u, 63u, 32u, 146u, 129u, 195u, 175u, 127u, 204u, 223u, 99u, 217u, 247u, 60u, 161u, 231u, 136u, 51u, 101u, 97u, 13u, 247u, 226u, 198u};
uint8_t var_126 [] = {15u, 13u, 14u, 20u, 6u, 5u, 8u, 19u, 4u, 10u, 3u, 1u};
uint8_t var_124 [] = {101u, 3u, 72u, 105u, 85u, 45u, 112u, 77u, 31u, 217u, 220u, 77u, 84u, 145u, 131u, 31u, 211u, 237u, 57u, 88u, 203u, 161u, 221u, 237u, 253u, 18u, 85u, 120u, 182u, 16u, 143u, 147u, 54u, 234u, 91u, 19u, 101u, 182u, 231u, 211u, 226u, 112u, 5u, 36u, 177u, 202u, 102u, 115u, 123u, 101u, 180u, 218u, 175u, 219u, 61u, 77u, 122u, 29u, 43u, 30u, 214u, 108u, 204u, 157u};
uint8_t var_123 [] = {4u, 5u, 2u, 6u};
uint8_t var_121 [] = {234u, 232u, 117u, 114u, 77u, 212u, 175u, 101u, 84u, 109u, 11u, 55u, 175u, 41u, 138u, 70u, 60u, 192u, 40u, 181u, 204u, 178u, 59u, 224u, 76u, 207u, 64u, 199u, 157u, 29u, 235u, 90u, 171u, 129u, 222u, 86u, 72u, 97u, 94u, 74u, 183u, 187u, 147u, 10u, 215u, 89u, 234u, 60u, 243u, 180u, 105u, 49u, 197u, 207u, 35u, 251u, 153u, 111u, 195u, 122u, 211u, 197u, 35u, 74u};
uint8_t var_120 [] = {1u, 17u, 15u, 2u, 0u, 16u, 10u, 3u, 7u, 12u};
uint8_t var_118 [] = {178u, 249u, 174u, 140u, 137u, 66u, 84u, 19u, 111u, 207u, 81u, 7u, 127u, 233u, 36u, 180u, 221u, 193u, 229u, 152u, 98u, 64u, 14u, 46u, 127u, 183u, 215u, 211u};
uint8_t var_117 [] = {208u, 222u, 1u, 14u, 38u, 246u, 148u, 212u, 43u, 72u, 250u, 255u, 222u, 205u, 231u, 215u, 188u, 55u, 223u, 110u, 248u, 126u, 249u, 63u, 245u, 108u, 209u, 188u, 234u, 61u, 42u, 169u, 231u, 185u, 235u, 30u, 237u, 0u, 129u, 75u, 172u, 30u, 206u, 241u, 124u, 78u, 214u, 130u, 190u, 127u, 24u, 225u, 164u, 84u, 247u, 198u, 240u, 113u, 22u, 22u, 88u, 242u, 188u, 106u};
uint8_t var_115 [] = {11u, 26u, 14u, 15u, 35u, 33u, 28u, 50u, 27u, 0u, 37u, 3u, 45u, 39u, 38u, 9u, 16u, 36u, 18u, 24u, 13u, 23u, 1u, 49u, 30u, 54u, 12u, 7u};
uint8_t var_113 [] = {216u, 191u, 218u, 203u, 109u, 137u, 15u, 175u, 196u, 58u, 213u, 149u, 181u, 77u, 239u, 150u, 214u, 161u, 167u, 167u, 83u, 52u, 254u, 195u, 159u, 186u, 46u, 112u, 173u, 74u, 78u, 215u, 239u, 208u, 184u, 238u, 146u, 94u, 237u, 250u, 191u, 150u, 91u, 143u, 133u, 251u, 89u, 153u, 181u, 197u, 45u, 242u, 4u, 179u, 51u, 155u, 2u, 145u, 194u, 183u, 117u, 46u, 237u, 93u};
uint8_t var_112 [] = {35u, 23u, 29u, 31u, 10u, 34u, 13u, 9u, 0u, 19u, 1u, 11u, 3u, 8u, 33u, 24u, 21u, 15u};
uint8_t var_110 [] = {138u, 33u, 49u, 97u, 45u, 198u, 56u, 240u, 252u, 219u, 200u, 209u, 137u, 195u, 92u};
uint8_t var_109 [] = {172u, 159u, 83u, 78u, 109u, 26u, 87u, 16u, 25u, 209u, 124u, 162u, 4u, 118u, 90u, 234u, 142u, 207u, 225u, 229u, 171u, 170u, 185u, 195u, 18u, 246u, 40u, 71u, 113u, 21u, 238u, 249u, 180u, 79u, 69u, 30u, 155u, 99u, 186u, 232u, 222u, 44u, 86u, 201u, 140u, 83u, 211u, 90u, 187u, 95u, 213u, 55u, 87u, 13u, 234u, 182u, 142u, 218u, 205u, 113u, 119u, 200u, 212u, 212u};
uint8_t var_107 [] = {11u, 14u, 17u, 21u, 13u, 28u, 0u, 4u, 25u, 19u, 10u, 12u, 24u, 23u, 22u};
uint8_t var_105 [] = {13u, 237u, 250u, 59u, 182u, 192u, 21u, 32u, 203u};
uint8_t var_104 [] = {159u, 134u, 70u, 95u, 95u, 48u, 103u, 117u, 231u, 24u, 133u, 183u, 113u, 102u, 29u, 201u, 25u, 165u, 193u, 228u, 218u, 71u, 75u, 70u, 229u, 34u, 169u, 0u, 153u, 225u, 29u, 188u, 91u, 124u, 51u, 133u, 74u, 135u, 145u, 161u, 5u, 16u, 159u, 184u, 207u, 228u, 146u, 219u, 135u, 122u, 70u, 63u, 53u, 20u, 137u, 143u, 109u, 121u, 26u, 29u, 158u, 80u, 50u, 221u};
uint8_t var_102 [] = {10u, 17u, 3u, 13u, 6u, 9u, 14u, 4u, 15u};
uint8_t var_100 [] = {25u, 133u, 241u, 41u, 211u};
uint8_t var_99 [] = {188u, 58u, 39u, 90u, 61u, 84u, 29u, 87u, 153u, 205u, 250u, 157u, 37u, 30u, 193u, 252u, 149u, 65u, 253u, 57u, 125u, 26u, 29u, 236u, 55u, 174u, 88u, 53u, 236u, 118u, 204u, 66u, 70u, 228u, 8u, 74u, 97u, 162u, 220u, 206u, 143u, 50u, 193u, 29u, 117u, 127u, 236u, 135u, 143u, 224u, 30u, 170u, 236u, 210u, 118u, 122u, 13u, 193u, 250u, 77u, 162u, 235u, 191u, 123u};
uint8_t var_97 [] = {6u, 8u, 4u, 7u, 2u};
uint8_t var_95 [] = {224u, 24u, 145u, 6u, 229u, 27u, 42u, 184u, 245u, 245u, 147u, 192u, 243u};
uint8_t var_94 [] = {161u, 9u, 158u, 187u, 105u, 240u, 194u, 23u, 106u, 167u, 254u, 214u, 118u, 117u, 108u, 55u, 112u, 101u, 37u, 170u, 107u, 163u, 81u, 21u, 239u, 77u, 211u, 159u, 1u, 231u, 118u, 233u, 97u, 229u, 24u, 35u, 33u, 113u, 169u, 198u, 192u, 22u, 103u, 8u, 176u, 225u, 132u, 22u, 195u, 77u, 229u, 177u, 38u, 149u, 49u, 70u, 33u, 139u, 198u, 128u, 246u, 221u, 105u, 211u};
uint8_t var_92 [] = {11u, 5u, 6u, 3u, 10u, 4u, 2u, 21u, 12u, 1u, 9u, 15u, 0u};
uint8_t var_90 [] = {55u, 37u, 107u, 142u, 199u, 169u, 227u, 62u, 33u, 124u};
uint8_t var_89 [] = {135u, 199u, 183u, 24u, 17u, 94u, 131u, 106u, 117u, 149u, 115u, 138u, 148u, 208u, 244u, 134u, 141u, 146u, 130u, 126u, 60u, 82u, 62u, 217u, 175u, 210u, 31u, 180u, 137u, 136u, 119u, 224u, 115u, 60u, 73u, 187u, 165u, 6u, 123u, 41u, 140u, 75u, 173u, 237u, 210u, 128u, 157u, 81u, 173u, 227u, 236u, 176u, 203u, 81u, 113u, 211u, 142u, 120u, 71u, 3u, 215u, 194u, 147u, 7u};
uint8_t var_87 [] = {9u, 13u, 14u, 10u, 0u, 16u, 1u, 7u, 12u, 6u};
uint8_t var_85 [] = {238u, 235u, 76u, 230u, 21u, 219u, 6u, 115u, 155u, 139u, 124u, 147u, 146u, 182u, 208u, 168u, 26u, 139u, 22u, 240u};
uint8_t var_84 [] = {10u, 14u, 156u, 49u, 181u, 26u, 151u, 177u, 158u, 78u, 105u, 182u, 74u, 178u, 91u, 167u, 169u, 137u, 49u, 116u, 82u, 49u, 174u, 213u, 41u, 252u, 238u, 82u, 127u, 181u, 204u, 139u, 139u, 80u, 225u, 143u, 181u, 128u, 187u, 110u, 157u, 132u, 218u, 206u, 0u, 101u, 226u, 175u, 46u, 227u, 57u, 15u, 219u, 103u, 165u, 35u, 243u, 191u, 90u, 217u, 152u, 19u, 91u, 133u};
uint8_t var_82 [] = {23u, 31u, 32u, 2u, 1u, 37u, 26u, 38u, 27u, 4u, 33u, 20u, 8u, 0u, 18u, 22u, 21u, 36u, 34u, 13u};
uint8_t var_80 [] = {51u, 84u, 251u, 79u, 169u, 178u, 97u, 33u, 90u, 0u, 91u, 158u, 224u, 205u, 75u, 28u, 3u, 218u, 110u, 90u};
uint8_t var_79 [] = {69u, 81u, 157u, 153u, 150u, 82u, 20u, 45u, 101u, 201u, 27u, 89u, 167u, 193u, 194u, 229u, 32u, 67u, 85u, 188u};
uint8_t var_78 [] = {7u, 86u, 55u, 184u, 15u, 158u, 72u, 233u, 133u, 233u};
uint8_t var_77 [] = {158u, 122u, 24u, 135u, 77u, 116u, 114u, 21u, 31u, 48u};
uint8_t var_76 [] = {8u, 244u, 149u, 106u, 11u, 74u, 10u, 105u, 180u, 241u, 53u, 11u, 14u};
uint8_t var_75 [] = {151u, 204u, 9u, 157u, 136u, 3u, 190u, 153u, 36u, 5u, 85u, 208u, 153u};
uint8_t var_74 [] = {163u, 112u, 39u, 165u, 124u};
uint8_t var_73 [] = {231u, 207u, 72u, 86u, 189u};
uint8_t var_72 [] = {124u, 139u, 130u, 178u, 131u, 94u, 153u, 139u, 0u};
uint8_t var_71 [] = {13u, 5u, 200u, 25u, 113u, 150u, 160u, 59u, 112u};
uint8_t var_70 [] = {43u, 165u, 118u, 143u, 24u, 178u, 108u, 42u, 168u, 165u, 210u, 11u, 242u, 151u, 76u};
uint8_t var_69 [] = {16u, 23u, 195u, 47u, 96u, 78u, 79u, 250u, 206u, 96u, 33u, 225u, 113u, 12u, 189u};
uint8_t var_68 [] = {102u, 129u, 39u, 76u, 226u, 43u, 174u, 239u, 160u, 251u, 130u, 253u, 210u, 44u, 57u, 197u, 38u, 134u};
uint8_t var_67 [] = {218u, 150u, 128u, 18u, 225u, 117u, 251u, 84u, 180u, 34u, 174u, 111u, 22u, 157u, 36u, 97u, 83u, 101u, 93u, 143u, 249u, 110u, 224u, 213u, 111u, 205u, 125u, 70u};
uint8_t var_66 [] = {228u, 220u, 11u, 216u, 206u, 139u, 123u, 42u, 253u, 119u, 29u, 75u, 19u, 94u, 198u, 208u, 223u, 53u, 17u, 77u, 216u, 164u, 148u, 183u, 197u, 129u, 52u, 30u};
uint8_t var_65 [] = {129u, 184u, 39u, 26u};
uint8_t var_64 [] = {165u, 172u, 17u, 195u};
uint8_t var_63 [] = {224u, 255u, 179u, 223u};
uint16_t var_62 [] = {2059u, 50136u, 19961u, 19085u};
uint8_t var_61 [] = {132u, 79u, 83u, 231u, 223u, 41u, 23u, 84u, 94u, 39u};
uint8_t var_60 [] = {64u, 170u, 250u, 169u, 211u, 59u, 143u, 124u, 179u, 165u, 180u, 8u, 238u, 216u, 187u, 137u, 252u, 103u};
uint8_t var_59 [] = {18u, 217u, 188u, 42u};
int32_t var_58 [] = {1549578886, 969508556, -1463606770, -570949145, 1223843346, 1203925732, -1372815026, -2071636721, -1347273561, 1979176278, -400677652, -1516821114, 1686741600, -1626075227, -557106289, -1989226629, 1303206559, -2019568180};
int16_t var_57 [] = {-17838, 21382, -24007, -28159, -7245, -8296, 4536, 4740, 31886, 7470, 6598, -11304, 5070, -7833, 19880, 30169, -19576, -29825};
int16_t var_56 [] = {27831, 17164, 25700, 20703, -9305, -13961, 3432, 31251, 31239, 2575, -3550, -22407, 22422, 3341, -11592, 3163, -18912, 24474};
int32_t var_55 [] = {-1410347872, -1203450804, 66771613, -223741775, -1141905884, 2138341856, -752061869, 736817258, -189959542, -1177808016, -864391040, -568227585, -1975568427, -1180804129, 119944555, 47776326, 1126587728, 445410999};
int32_t var_54 [] = {193247136, 856811342, -1426659505, -602051769, -1581588484, 586962190, 2128307703, -680727552, -1153985669, -2086855987, 1290791744, -1617390193, -828132707, -749102283, 471920188, 875051716, -1197869197, -1799830041};
size_t var_53 = 14u;
int32_t var_52 = 33732954;
uint8_t var_51 = 68u;
uint16_t var_50 = 28439u;
__riscv_vsetvl_e8m2(4);
vuint8m2_t var_128 = __riscv_vle8_v_u8m2(var_129, 4);
// 4, 5, 2, 3

__riscv_vsetvl_e8m2(12);
vuint8m2_t var_125 = __riscv_vle8_v_u8m2(var_126, 12);
// 15, 13, 14, 20, 6, 5, 8, 19, 4, 10, 3, 1

__riscv_vsetvl_e8m2(4);
vuint8m2_t var_122 = __riscv_vle8_v_u8m2(var_123, 4);
// 4, 5, 2, 6

__riscv_vsetvl_e8m2(10);
vuint8m2_t var_119 = __riscv_vle8_v_u8m2(var_120, 10);
// 1, 17, 15, 2, 0, 16, 10, 3, 7, 12

__riscv_vsetvl_e8m2(28);
vuint8m2_t var_114 = __riscv_vle8_v_u8m2(var_115, 28);
// 11, 26, 14, 15, 35, 33, 28, 50, 27, 0, 37, 3, 45, 39, 38, 9, 16, 36, 18, 24, 13, 23, 1, 49, 30, 54, 12, 7

__riscv_vsetvl_e8m2(18);
vuint8m2_t var_111 = __riscv_vle8_v_u8m2(var_112, 18);
// 35, 23, 29, 31, 10, 34, 13, 9, 0, 19, 1, 11, 3, 8, 33, 24, 21, 15

__riscv_vsetvl_e8m2(15);
vuint8m2_t var_106 = __riscv_vle8_v_u8m2(var_107, 15);
// 11, 14, 17, 21, 13, 28, 0, 4, 25, 19, 10, 12, 24, 23, 22

__riscv_vsetvl_e8m2(9);
vuint8m2_t var_101 = __riscv_vle8_v_u8m2(var_102, 9);
// 10, 17, 3, 13, 6, 9, 14, 4, 15

__riscv_vsetvl_e8m2(5);
vuint8m2_t var_96 = __riscv_vle8_v_u8m2(var_97, 5);
// 6, 8, 4, 7, 2

__riscv_vsetvl_e8m2(13);
vuint8m2_t var_91 = __riscv_vle8_v_u8m2(var_92, 13);
// 11, 5, 6, 3, 10, 4, 2, 21, 12, 1, 9, 15, 0

__riscv_vsetvl_e8m2(10);
vuint8m2_t var_86 = __riscv_vle8_v_u8m2(var_87, 10);
// 9, 13, 14, 10, 0, 16, 1, 7, 12, 6

__riscv_vsetvl_e8m2(20);
vuint8m2_t var_81 = __riscv_vle8_v_u8m2(var_82, 20);
// 23, 31, 32, 2, 1, 37, 26, 38, 27, 4, 33, 20, 8, 0, 18, 22, 21, 36, 34, 13

vuint8m2_t var_20 = __riscv_vle8_v_u8m2(var_80, 20);
// 51, 84, 251, 79, 169, 178, 97, 33, 90, 0, 91, 158, 224, 205, 75, 28, 3, 218, 110, 90

vuint8m2_t var_21 = __riscv_vle8_v_u8m2(var_79, 20);
// 69, 81, 157, 153, 150, 82, 20, 45, 101, 201, 27, 89, 167, 193, 194, 229, 32, 67, 85, 188

__riscv_vsetvl_e8m2(10);
vuint8m2_t var_22 = __riscv_vle8_v_u8m2(var_78, 10);
// 7, 86, 55, 184, 15, 158, 72, 233, 133, 233

vuint8m2_t var_23 = __riscv_vle8_v_u8m2(var_77, 10);
// 158, 122, 24, 135, 77, 116, 114, 21, 31, 48

__riscv_vsetvl_e8m2(13);
vuint8m2_t var_24 = __riscv_vle8_v_u8m2(var_76, 13);
// 8, 244, 149, 106, 11, 74, 10, 105, 180, 241, 53, 11, 14

vuint8m2_t var_25 = __riscv_vle8_v_u8m2(var_75, 13);
// 151, 204, 9, 157, 136, 3, 190, 153, 36, 5, 85, 208, 153

__riscv_vsetvl_e8m2(5);
vuint8m2_t var_26 = __riscv_vle8_v_u8m2(var_74, 5);
// 163, 112, 39, 165, 124

vuint8m2_t var_27 = __riscv_vle8_v_u8m2(var_73, 5);
// 231, 207, 72, 86, 189

__riscv_vsetvl_e8m2(9);
vuint8m2_t var_28 = __riscv_vle8_v_u8m2(var_72, 9);
// 124, 139, 130, 178, 131, 94, 153, 139, 0

vuint8m2_t var_29 = __riscv_vle8_v_u8m2(var_71, 9);
// 13, 5, 200, 25, 113, 150, 160, 59, 112

__riscv_vsetvl_e8m2(15);
vuint8m2_t var_30 = __riscv_vle8_v_u8m2(var_70, 15);
// 43, 165, 118, 143, 24, 178, 108, 42, 168, 165, 210, 11, 242, 151, 76

vuint8m2_t var_31 = __riscv_vle8_v_u8m2(var_69, 15);
// 16, 23, 195, 47, 96, 78, 79, 250, 206, 96, 33, 225, 113, 12, 189

__riscv_vsetvl_e8m2(18);
vuint8m2_t var_32 = __riscv_vle8_v_u8m2(var_68, 18);
// 102, 129, 39, 76, 226, 43, 174, 239, 160, 251, 130, 253, 210, 44, 57, 197, 38, 134

__riscv_vsetvl_e8m2(28);
vuint8m2_t var_33 = __riscv_vle8_v_u8m2(var_67, 28);
// 218, 150, 128, 18, 225, 117, 251, 84, 180, 34, 174, 111, 22, 157, 36, 97, 83, 101, 93, 143, 249, 110, 224, 213, 111, 205, 125, 70

vuint8m2_t var_34 = __riscv_vle8_v_u8m2(var_66, 28);
// 228, 220, 11, 216, 206, 139, 123, 42, 253, 119, 29, 75, 19, 94, 198, 208, 223, 53, 17, 77, 216, 164, 148, 183, 197, 129, 52, 30

__riscv_vsetvl_e8m1(4);
vuint8m1_t var_36 = __riscv_vle8_v_u8m1(var_65, 4);
// 129, 184, 39, 26

__riscv_vsetvl_e8mf4(4);
vuint8mf4_t var_37 = __riscv_vle8_v_u8mf4(var_64, 4);
// 165, 172, 17, 195

__riscv_vsetvl_e8m1(4);
vuint8m1_t var_38 = __riscv_vle8_v_u8m1(var_63, 4);
// 224, 255, 179, 223

__riscv_vsetvl_e16mf2(4);
vuint16mf2_t var_39 = __riscv_vle16_v_u16mf2(var_62, 4);
// 2059, 50136, 19961, 19085

__riscv_vsetvl_e8m2(10);
vuint8m2_t var_40 = __riscv_vle8_v_u8m2(var_61, 10);
// 132, 79, 83, 231, 223, 41, 23, 84, 94, 39

__riscv_vsetvl_e8m2(18);
vuint8m2_t var_41 = __riscv_vle8_v_u8m2(var_60, 18);
// 64, 170, 250, 169, 211, 59, 143, 124, 179, 165, 180, 8, 238, 216, 187, 137, 252, 103

__riscv_vsetvl_e8m2(4);
vuint8m2_t var_42 = __riscv_vle8_v_u8m2(var_59, 4);
// 18, 217, 188, 42

__riscv_vsetvl_e32m8(18);
vint32m8_t var_45 = __riscv_vle32_v_i32m8(var_58, 18);
// 1549578886, 969508556, -1463606770, -570949145, 1223843346, 1203925732, -1372815026, -2071636721, -1347273561, 1979176278, -400677652, -1516821114, 1686741600, -1626075227, -557106289, -1989226629, 1303206559, -2019568180

vint16m4_t var_46 = __riscv_vle16_v_i16m4(var_57, 18);
// -17838, 21382, -24007, -28159, -7245, -8296, 4536, 4740, 31886, 7470, 6598, -11304, 5070, -7833, 19880, 30169, -19576, -29825

vint16m4_t var_47 = __riscv_vle16_v_i16m4(var_56, 18);
// 27831, 17164, 25700, 20703, -9305, -13961, 3432, 31251, 31239, 2575, -3550, -22407, 22422, 3341, -11592, 3163, -18912, 24474

vint32m8_t var_48 = __riscv_vle32_v_i32m8(var_55, 18);
// -1410347872, -1203450804, 66771613, -223741775, -1141905884, 2138341856, -752061869, 736817258, -189959542, -1177808016, -864391040, -568227585, -1975568427, -1180804129, 119944555, 47776326, 1126587728, 445410999

vint32m8_t var_49 = __riscv_vle32_v_i32m8(var_54, 18);
// 193247136, 856811342, -1426659505, -602051769, -1581588484, 586962190, 2128307703, -680727552, -1153985669, -2086855987, 1290791744, -1617390193, -828132707, -749102283, 471920188, 875051716, -1197869197, -1799830041

__riscv_vsetvl_e8m2(20);
__riscv_vse8_v_u8m2(var_85, var_20, 20);
__riscv_vsuxei8_v_u8m2(var_84, var_81, var_21, 20);
__riscv_vsetvl_e8m2(10);
__riscv_vse8_v_u8m2(var_90, var_22, 10);
__riscv_vsuxei8_v_u8m2(var_89, var_86, var_23, 10);
__riscv_vsetvl_e8m2(13);
__riscv_vse8_v_u8m2(var_95, var_24, 13);
__riscv_vsuxei8_v_u8m2(var_94, var_91, var_25, 13);
__riscv_vsetvl_e8m2(5);
__riscv_vse8_v_u8m2(var_100, var_26, 5);
__riscv_vsuxei8_v_u8m2(var_99, var_96, var_27, 5);
__riscv_vsetvl_e8m2(9);
__riscv_vse8_v_u8m2(var_105, var_28, 9);
__riscv_vsuxei8_v_u8m2(var_104, var_101, var_29, 9);
__riscv_vsetvl_e8m2(15);
__riscv_vse8_v_u8m2(var_110, var_30, 15);
__riscv_vsuxei8_v_u8m2(var_109, var_106, var_31, 15);
__riscv_vsetvl_e8m2(18);
vuint8m2_t var_13 = __riscv_vor_vx_u8m2(var_32, var_51, 18);
// 102, 197, 103, 76, 230, 111, 238, 239, 228, 255, 198, 253, 214, 108, 125, 197, 102, 198

__riscv_vsetvl_e8m2(28);
__riscv_vse8_v_u8m2(var_118, var_33, 28);
__riscv_vsuxei8_v_u8m2(var_117, var_114, var_34, 28);
__riscv_vsetvl_e16mf2(4);
vbool32_t var_35 = __riscv_vmseq_vx_u16mf2_b32(var_39, var_50, 4);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

__riscv_vsetvl_e8m2(10);
__riscv_vsuxei8_v_u8m2(var_121, var_119, var_40, 10);
__riscv_vsetvl_e16m4(18);
vuint16m4_t var_8 = __riscv_vzext_vf2_u16m4(var_41, 18);
// 64, 170, 250, 169, 211, 59, 143, 124, 179, 165, 180, 8, 238, 216, 187, 137, 252, 103

__riscv_vsetvl_e8m2(4);
__riscv_vsuxei8_v_u8m2(var_124, var_122, var_42, 4);
__riscv_vsetvl_e16m4(18);
vbool4_t var_44 = __riscv_vmsgt_vv_i16m4_b4(var_46, var_47, 18);
// 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

vbool4_t var_43 = __riscv_vmseq_vv_i32m8_b4(var_48, var_49, 18);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

if(!check(var_85, var_170, var_171)) {cerr << "check 169 fails" << endl; return_value = 1;}
if(!check(var_84, var_167, var_168)) {cerr << "check 166 fails" << endl; return_value = 1;}
if(!check(var_90, var_173, var_174)) {cerr << "check 172 fails" << endl; return_value = 1;}
if(!check(var_89, var_164, var_165)) {cerr << "check 163 fails" << endl; return_value = 1;}
if(!check(var_95, var_176, var_177)) {cerr << "check 175 fails" << endl; return_value = 1;}
if(!check(var_94, var_161, var_162)) {cerr << "check 160 fails" << endl; return_value = 1;}
if(!check(var_100, var_179, var_180)) {cerr << "check 178 fails" << endl; return_value = 1;}
if(!check(var_99, var_158, var_159)) {cerr << "check 157 fails" << endl; return_value = 1;}
if(!check(var_105, var_182, var_183)) {cerr << "check 181 fails" << endl; return_value = 1;}
if(!check(var_104, var_155, var_156)) {cerr << "check 154 fails" << endl; return_value = 1;}
if(!check(var_110, var_185, var_186)) {cerr << "check 184 fails" << endl; return_value = 1;}
if(!check(var_109, var_152, var_153)) {cerr << "check 151 fails" << endl; return_value = 1;}
__riscv_vsuxei8_v_u8m2(var_113, var_111, var_13, 18);
if(!check(var_118, var_188, var_189)) {cerr << "check 187 fails" << endl; return_value = 1;}
if(!check(var_117, var_146, var_147)) {cerr << "check 145 fails" << endl; return_value = 1;}
__riscv_vsetvl_e8mf4(4);
vuint8m1_t var_10 = __riscv_vredand_vs_u8mf4_u8m1_tum(var_35, var_36, var_37, var_38, 4);
// 224

if(!check(var_121, var_143, var_144)) {cerr << "check 142 fails" << endl; return_value = 1;}
__riscv_vsetvl_e16m4(18);
vbool4_t var_3 = __riscv_vmsbc_vv_u16m4_b4(var_8, var_8, 18);
// 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

if(!check(var_124, var_140, var_141)) {cerr << "check 139 fails" << endl; return_value = 1;}
vbool4_t var_6 = __riscv_vmseq_vx_i32m8_b4_mu(var_43, var_44, var_45, var_52, 18);
// 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

if(!check(var_113, var_149, var_150)) {cerr << "check 148 fails" << endl; return_value = 1;}
__riscv_vsetvl_e8m1(1);
uint8_t var_5 = __riscv_vmv_x_s_u8m1_u8(var_10);
// 224

__riscv_vsetvl_e8m2(18);
vuint8m2_t var_4 = __riscv_vmadd_vx_u8m2_mu(var_6, var_13, var_5, var_13, 18);
// 102, 37, 103, 76, 38, 143, 46, 239, 100, 31, 6, 93, 214, 108, 221, 37, 102, 198

__riscv_vsetvl_e8m2(12);
__riscv_vsuxei8_v_u8m2(var_127, var_125, var_4, 12);
__riscv_vsetvl_e8m2(18);
vuint8m2_t var_2 = __riscv_vslidedown_vx_u8m2_mu(var_3, var_4, var_4, var_53, 18);
// 102, 37, 103, 76

__riscv_vsetvl_e8m2(4);
__riscv_vsuxei8_v_u8m2(var_131, var_128, var_4, 4);
if(!check(var_127, var_137, var_138)) {cerr << "check 136 fails" << endl; return_value = 1;}
__riscv_vse8_v_u8m2(var_132, var_2, 4);
if(!check(var_131, var_134, var_135)) {cerr << "check 133 fails" << endl; return_value = 1;}
if(!check(var_132, var_191, var_192)) {cerr << "check 190 fails" << endl; return_value = 1;}
if (return_value)
  __builtin_abort ();
return return_value;
}
