// { dg-do compile { target { aarch64*-*-* arm*-*-* } } }
// { dg-options "-Wno-abi" }
// { dg-require-effective-target arm_eabi { target arm*-*-* } }

// AAPCS \S 7.1.4 requires that va_list be a typedef for "struct
// __va_list".  The mangling is as if it were "std::__va_list".
// AAPCS64 \S 7.1.4 has the same requirement for AArch64 targets.
// #include <stdarg.h>
typedef __builtin_va_list va_list;

// { dg-final { scan-assembler "\n_Z1fPSt9__va_list:" } }
void f(va_list*) {}

// { dg-final { scan-assembler "\n_Z1gSt9__va_listS_:" } }
void g(va_list, va_list) {}
