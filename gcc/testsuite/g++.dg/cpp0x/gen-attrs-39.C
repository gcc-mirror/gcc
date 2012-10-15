// PR debug/43370
// { dg-do compile { target c++11 } }
// { dg-options "-g" }

int fragile_block(void) {
  typedef struct [[gnu::aligned (16)]] {
    int i;
  } XmmUint16;
  return 0;
}
