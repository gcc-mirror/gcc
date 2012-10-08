// { dg-do compile { target c++11 } }

int fragile_block(void) {
  typedef 
  [[gnu::aligned (16)]] // { dg-warning "ignored" }
  struct  {
    int i;
  } XmmUint16;
  return 0;
}
