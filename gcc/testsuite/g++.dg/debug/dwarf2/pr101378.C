// PR debug/101378
// { dg-do compile { target c++11 } }
// { dg-options "-gdwarf-5 -dA" }
// { dg-final { scan-assembler-times "0\[^0-9x\\r\\n\]* DW_AT_data_member_location" 1 } }
// { dg-final { scan-assembler-times "1\[^0-9x\\r\\n\]* DW_AT_data_member_location" 1 } }
// { dg-final { scan-assembler-times "2\[^0-9x\\r\\n\]* DW_AT_data_member_location" 1 } }
// { dg-final { scan-assembler-not "-1\[^0-9x\\r\\n\]* DW_AT_data_member_location" } }

struct E {};
struct S
{
  [[no_unique_address]] E e, f, g;
} s;
