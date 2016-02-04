// PR c++/68979
// { dg-do compile { target int32 } }
// { dg-options "-fpermissive -Wno-shift-overflow -Wno-shift-count-overflow -Wno-shift-count-negative" }

enum A { AA = -1 << 4 }; // { dg-warning "operand of shift expression" "" { target c++11 } }
enum B { BB = 1 << -4 }; // { dg-warning "operand of shift expression" }
enum C { CC = 1 << __SIZEOF_INT__ * 4 * __CHAR_BIT__ - 4 }; // { dg-warning "operand of shift expression" }
enum D { DD = 10 << __SIZEOF_INT__ * __CHAR_BIT__ - 2 }; // { dg-warning "shift expression" "" { target c++11 } }
