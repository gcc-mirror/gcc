// { dg-do compile { target c++11 } }

int fragile_block(void) {
  typedef 
  [[gnu::aligned (16)]] // { dg-error "standard attributes in middle of decl-specifiers" }
// { dg-warning "attribute ignored" "" { target *-*-* } .-1 }
  struct  {
    int i;
  } XmmUint16;
  return 0;
}
