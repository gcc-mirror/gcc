#include <assert.h>
#include <cstddef>
#include <cstdint>

void testnull(std::nullptr_t n)
{
  assert(n == nullptr);
}

void testnullnull(std::nullptr_t n1, std::nullptr_t n2)
{
  assert(n1 == nullptr);
  assert(n2 == nullptr);
}

/****************************************/
// https://issues.dlang.org/show_bug.cgi?id=19658

enum class i8_19658 : std::int8_t;
enum class u8_19658 : std::uint8_t;
enum class i16_19658 : std::int16_t;
enum class u16_19658 : std::uint16_t;
enum class i32_19658 : std::int32_t;
enum class u32_19658 : std::uint32_t;
enum class i64_19658 : std::int64_t;
enum class u64_19658 : std::uint64_t;

void test19658_i8(i8_19658) {}
void test19658_u8(u8_19658) {}
void test19658_i16(i16_19658) {}
void test19658_u16(u16_19658) {}
void test19658_i32(i32_19658) {}
void test19658_u32(u32_19658) {}
void test19658_i64(i64_19658) {}
void test19658_u64(u64_19658) {}
