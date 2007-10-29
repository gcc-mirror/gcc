// PR c++/30659

extern "C" template A<char> foo(); // { dg-error "forbids|static data|expected" }
