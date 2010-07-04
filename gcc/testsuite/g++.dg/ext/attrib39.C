// PR debug/43370
// { dg-options "-g" }

int fragile_block(void) {
  typedef __attribute__ ((aligned (16))) struct {
    int i;
  } XmmUint16;
  return 0;
}
