// Bug: g++ fails to instantiate operator<<.
// Build don't run:
// Special g++ Options: -static
// Skip if not target: i?86-*-linux*

// libc-5.4.xx has __IO_putc in its static C library, which can conflict
// with the copy of __IO_putc in the libstdc++ library built by egcs.
#include <iostream.h>
#include <streambuf.h>

istream x;
extern "C" int putc(), fgets();

main () {
  x.get();
  putc();
  fgets(); 
  x.gets(0, 0);
}

