/* { dg-options "-msse2" } */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */

void foo1(int i, int j) __attribute__((cdecl, regparm(2)));
void foo2(int i, int j) __attribute__((stdcall, regparm(2)));
void foo3(int i, int j) __attribute__((fastcall, regparm(2))); /* { dg-error "not compatible" } */
void foo4(int i, int j) __attribute__((thiscall, regparm(2))); /* { dg-error "not compatible" } */
void foo5(int i, int j) __attribute__((sseregparm, regparm(2)));

void foo6(int i, int j) __attribute__((stdcall, fastcall)); /* { dg-error "not compatible" } */
void foo7(int i, int j) __attribute__((regparm(2), fastcall)); /* { dg-error "not compatible" } */
void foo8(int i, int j) __attribute__((cdecl, fastcall)); /* { dg-error "not compatible" } */
void foo9(int i, int j) __attribute__((thiscall, fastcall)); /* { dg-error "not compatible" } */
void foo10(int i, int j) __attribute__((sseregparm, fastcall));

void foo11(int i, int j) __attribute__((cdecl, stdcall)); /* { dg-error "not compatible" } */
void foo12(int i, int j) __attribute__((fastcall, stdcall)); /* { dg-error "not compatible" } */
void foo13(int i, int j) __attribute__((thiscall, stdcall)); /* { dg-error "not compatible" } */
void foo14(int i, int j) __attribute__((regparm(2), stdcall));
void foo15(int i, int j) __attribute__((sseregparm, stdcall));

void foo16(int i, int j) __attribute__((stdcall, cdecl)); /* { dg-error "not compatible" } */
void foo17(int i, int j) __attribute__((fastcall, cdecl)); /* { dg-error "not compatible" } */
void foo18(int i, int j) __attribute__((thiscall, cdecl)); /* { dg-error "not compatible" } */
void foo19(int i, int j) __attribute__((regparm(2), cdecl));
void foo20(int i, int j) __attribute__((sseregparm, cdecl));

void foo21(int i, int j) __attribute__((stdcall, thiscall)); /* { dg-error "not compatible" } */
void foo22(int i, int j) __attribute__((fastcall, thiscall)); /* { dg-error "not compatible" } */
void foo23(int i, int j) __attribute__((cdecl, thiscall)); /* { dg-error "not compatible" } */
void foo24(int i, int j) __attribute__((regparm(2), thiscall)); /* { dg-error "not compatible" } */
void foo25(int i, int j) __attribute__((sseregparm, thiscall));

void foo26(int i, int j) __attribute__((cdecl, sseregparm));
void foo27(int i, int j) __attribute__((fastcall, sseregparm));
void foo28(int i, int j) __attribute__((stdcall, sseregparm));
void foo29(int i, int j) __attribute__((thiscall, sseregparm));
void foo30(int i, int j) __attribute__((regparm(2), sseregparm));

