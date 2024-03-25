/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O1" } */

typedef          int i32;
typedef unsigned int u32;

static inline void write_i32(void *memory, i32 value) {
  // swap i32 bytes as if it was u32:
  u32 u_value = value;
  value = __builtin_bswap32(u_value);

  // llvm infers '1' alignment from destination type
  __builtin_memcpy(__builtin_assume_aligned(memory, 1), &value, sizeof(value));
}

__attribute__((noipa))
static void bug (void) {
  #define assert_eq(lhs, rhs) if (lhs != rhs) __builtin_trap()

  unsigned char data[5];
  write_i32(data, -1362446643);
  assert_eq(data[0], 0xAE);
  assert_eq(data[1], 0xCA);
  write_i32(data + 1, -1362446643);
  assert_eq(data[1], 0xAE);
}

int main() {
    bug();
    return 0;
}
