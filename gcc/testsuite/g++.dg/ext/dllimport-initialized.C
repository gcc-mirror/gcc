// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw* x86_64-*-mingw* } }

__attribute__((dllimport)) int i __attribute__((unused)) = 0;  // { dg-error "32:definition of .int i. is marked .dllimport." }
