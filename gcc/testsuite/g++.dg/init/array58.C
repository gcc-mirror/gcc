/* PR c++/94510 - nullptr_t implicitly cast to zero twice in std::array
   { dg-do compile } */

int ia1[2] = { (void*)0 };              // { dg-error "invalid conversion from 'void\\\*'" }
int ia2[2] = { (void*)0, 0 };           // { dg-error "invalid conversion from 'void\\\*'" }
int ia3[] = { (void*)0, 0 };            // { dg-error "invalid conversion from 'void\\\*'" }

int ia4[2] = { __null };                // { dg-warning "\\\[-Wconversion-null" }
int ia5[2] = { __null, 0 };             // { dg-warning "\\\[-Wconversion-null" }
int ia6[] = { __null, 0 };              // { dg-warning "\\\[-Wconversion-null" }


const char ca1[2] = { (char*)0, 0 };    // { dg-error "invalid conversion from 'char\\\*'" }

const char ca2[2] = { __null, 0 };      // { dg-warning "\\\[-Wconversion-null" }


typedef void Func ();
const char ca6[2] = { (Func*)0, 0 };    // { dg-error "invalid conversion from 'void \\\(\\\*\\\)\\\(\\\)' to 'char'" }

struct S;
typedef int S::*MemPtr;
typedef int (S::*MemFuncPtr)();

const char ca4[2] = { (MemPtr)0, 0 };   // { dg-error "cannot convert 'MemPtr' " }
const char ca5[2] = { (MemFuncPtr)0, 0 };   // { dg-error "cannot convert 'int \\\(S::\\\*\\\)\\\(\\\)' "  }
