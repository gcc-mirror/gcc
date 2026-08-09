/* { dg-do compile { target i?86-*-mingw32* i?86-*-cygwin* } } */
/* { dg-options "-fvisibility=hidden" } */

void some_cdecl (int, int) {}
void __attribute__((stdcall)) some_stdcall (int, int) {}
void __attribute__((fastcall)) some_fastcall (int, int) {}

/* Hidden visibility on 32-bit PE/COFF drops the user label prefix in
   .drectve, but keeps stdcall and fastcall decoration.  */
/* { dg-final { scan-assembler {-exclude-symbols:some_cdecl} } } */
/* { dg-final { scan-assembler {-exclude-symbols:some_stdcall@8} } } */
/* { dg-final { scan-assembler {-exclude-symbols:@some_fastcall@8} } } */
