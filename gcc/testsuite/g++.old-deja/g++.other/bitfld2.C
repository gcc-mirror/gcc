// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options: -funsigned-bitfields

typedef int i[4];

struct S {
  i j:12; // ERROR - array type as bitfield
};
