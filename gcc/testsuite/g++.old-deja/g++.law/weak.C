// { dg-do link { target i?86-*-linux* } }
// { dg-options "-static" }
// Bug: g++ fails to instantiate operator<<.

// libc-5.4.xx has __IO_putc in its static C library, which can conflict
// with the copy of __IO_putc in the libstdc++ library built by egcs.
#include <iostream>
#include <streambuf>
#include <cstdio>

std::istream x (0);

main () {
  x.get();
  std::putc(0, 0);
  std::fgets(0, 0, 0); 
  x.get((char*) 0, 0);
}

