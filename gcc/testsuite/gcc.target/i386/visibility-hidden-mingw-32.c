/* { dg-do compile { target i?86-*-mingw32* i?86-*-cygwin* } } */
/* { dg-options "-fvisibility=hidden" } */

void some_cdecl (int, int) {}
void __attribute__((stdcall)) some_stdcall (int, int) {}
void __attribute__((fastcall)) some_fastcall (int, int) {}

void _prefixed_cdecl (int, int) {}
void __attribute__((stdcall)) _prefixed_stdcall (int, int) {}
void __attribute__((fastcall)) _prefixed_fastcall (int, int) {}

void with_custom_symbol (int, int) __asm__("_custom_symbol");
void with_custom_symbol (int, int) {}

/* Hidden visibility on 32-bit PE/COFF drops the user label prefix in
   .drectve, but keeps stdcall and fastcall decoration.  */
/* { dg-final { scan-assembler {-exclude-symbols:some_cdecl} } } */
/* { dg-final { scan-assembler {-exclude-symbols:some_stdcall@8} } } */
/* { dg-final { scan-assembler {-exclude-symbols:@some_fastcall@8} } } */
/* { dg-final { scan-assembler {-exclude-symbols:_prefixed_cdecl} } } */
/* { dg-final { scan-assembler {-exclude-symbols:_prefixed_stdcall@8} } } */
/* { dg-final { scan-assembler {-exclude-symbols:@_prefixed_fastcall@8} } } */
/* { dg-final { scan-assembler {-exclude-symbols:custom_symbol} } } */
