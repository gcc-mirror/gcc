// { dg-options "-fmodules-atom" }

#ifndef __cpp_modules_atom
#error "Wat?"
#endif

#ifdef __cpp_modules_ts
#error "Wat?"
#endif

export module bob;
// { dg-module-bmi "bob" }
