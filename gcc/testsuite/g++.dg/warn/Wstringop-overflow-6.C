/* PR middle-end/97595 - bogus -Wstringop-overflow due to DECL_SIZE_UNIT
   underreporting field size
   { dg-do compile { target c++11 } }
   { dg-options "-O2 -Wall -Wsystem-headers" } */

#include <iostream>

template void std::basic_iostream<char>::swap (basic_iostream&);
