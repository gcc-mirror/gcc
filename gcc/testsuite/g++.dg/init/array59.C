/* PR c++/94510 - nullptr_t implicitly cast to zero twice in std::array
   { dg-do compile { target c++11 } } */

namespace std {
typedef __typeof__ (nullptr) nullptr_t;
}

int ia1[2] = { nullptr };                 // { dg-error "cannot convert 'std::nullptr_t' to 'int'" }
int ia2[2] = { nullptr, 0 };              // { dg-error "cannot convert 'std::nullptr_t' to 'int'" }
int ia3[] = { nullptr, 0 };               // { dg-error "cannot convert 'std::nullptr_t' to 'int'" }

int ia4[2] = { (std::nullptr_t)0 };      // { dg-error "cannot convert 'std::nullptr_t' to 'int'" }
int ia5[2] = { (std::nullptr_t)0, 0 };   // { dg-error "cannot convert 'std::nullptr_t' to 'int'" }
int ia6[] = { (std::nullptr_t)0, 0 };    // { dg-error "cannot convert 'std::nullptr_t' to 'int'" }


const char ca1[2] = { nullptr, 0 };       // { dg-error "cannot convert 'std::nullptr_t' to 'const char'" }

const char ca2[2] = { (char*)nullptr, 0 };// { dg-error "invalid conversion from 'char\\\*' to 'char'" }

const char ca3[2] = { std::nullptr_t () };// { dg-error "cannot convert 'std::nullptr_t'" }

/* Verify that arrays of member pointers can be initialized by a literal
   zero as well as nullptr.  */

struct S { };
typedef int S::*MemPtr;
typedef int (S::*MemFuncPtr)();

MemPtr mp1[3] = { 0, nullptr, (MemPtr)0 };
MemPtr mp2[3] = { 0, std::nullptr_t (), MemPtr () };

MemPtr mp3[3] = { 0, (void*)0 };          // { dg-error "cannot convert 'void\\\*' to 'MemPtr' " }
MemPtr mp4[3] = { 0, (S*)0 };             // { dg-error "cannot convert 'S\\\*' to 'MemPtr' " }
MemPtr mp5[3] = { 0, S () };              // { dg-error "cannot convert 'S' to 'MemPtr' " }

MemFuncPtr mfp1[3] = { 0, nullptr, (MemFuncPtr)0 };
MemFuncPtr mfp2[3] = { 0, std::nullptr_t (), MemFuncPtr () };

MemFuncPtr mfp3[3] = { 0, (void*)0 };     // { dg-error "cannot convert 'void\\\*' to 'MemFuncPtr' " }
MemFuncPtr mfp4[3] = { 0, (S*)0 };        // { dg-error "cannot convert 'S\\\*' to 'MemFuncPtr' " }
MemFuncPtr mfp5[3] = { 0, S () };         // { dg-error "cannot convert 'S' to 'MemFuncPtr' " }
