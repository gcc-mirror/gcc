// PR c++/27748
// This is supposed to succeed only if
// the target defines HANDLE_PRAGMA_PACK_PUSH_POP 
// and doesn't define HANDLE_PRAGMA_PACK_WITH_EXPANSION.
// { dg-do compile { target { ! { *-*-solaris2* sh*-[us]*-elf } } } }

#define push bar
#define foo _Pragma ("pack(push)")
foo
int i;
#pragma pack(pop)
