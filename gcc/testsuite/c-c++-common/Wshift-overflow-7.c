/* PR c++/55095 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-Wshift-overflow=2" } */

int i00 = 0b1 << 31; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i01 = 0b10 << 30; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i02 = 0b100 << 29; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i03 = 0b1000 << 28; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i04 = 0b10000 << 27; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i05 = 0b100000 << 26; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i06 = 0b1000000 << 25; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i07 = 0b10000000 << 24; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i08 = 0b100000000 << 23; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i09 = 0b1000000000 << 22; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i10 = 0b10000000000 << 21; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i11 = 0b100000000000 << 20; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i12 = 0b1000000000000 << 19; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i13 = 0b10000000000000 << 18; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i14 = 0b100000000000000 << 17; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i15 = 0b1000000000000000 << 16; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i16 = 0b10000000000000000 << 15; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i17 = 0b100000000000000000 << 14; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i18 = 0b1000000000000000000 << 13; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i19 = 0b10000000000000000000 << 12; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i20 = 0b100000000000000000000 << 11; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i21 = 0b1000000000000000000000 << 10; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i22 = 0b10000000000000000000000 << 9; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i23 = 0b100000000000000000000000 << 8; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i24 = 0b1000000000000000000000000 << 7; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i25 = 0b10000000000000000000000000 << 6; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i26 = 0b100000000000000000000000000 << 5; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i27 = 0b1000000000000000000000000000 << 4; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i28 = 0b10000000000000000000000000000 << 3; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i29 = 0b100000000000000000000000000000 << 2; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i30 = 0b1000000000000000000000000000000 << 1; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++11_down } } } */
int i31 = (int) 0b10000000000000000000000000000000u << 1; /* { dg-warning "requires 33 bits to represent" "" { target { c || c++17_down } } } */
