// PR c++/30659

extern "C" template A<char> foo();
// { dg-error "forbids|static data|expected|template" "" { target c++17_down } .-1 }
// { dg-error "permitted|forbids|static data|expected|template" "" { target c++20 } .-2 }
