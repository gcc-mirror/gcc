/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f -mabi=lp64d -O2 -mrvv-max-lmul=m4 -mrvv-vector-bits=zvl" } */

typedef unsigned char u8;

__attribute__((noipa))
static void check(const u8 * v) {
    if (*v != 15) __builtin_trap();
}

__attribute__((noipa))
static void bug(void) {
    u8 in_lanes[32];
    for (unsigned i = 0; i < 32; i += 2) {
      in_lanes[i + 0] = 0;
      in_lanes[i + 1] = ((u8)0xff) >> (i & 7);
    }

    check(&in_lanes[13]);
  }

int main() {
    bug();
}
