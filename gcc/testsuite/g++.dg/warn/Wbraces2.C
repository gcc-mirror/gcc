// PR c++/20175
// { dg-options "-Wmissing-braces" }
int a[2][2] = { 0, 1, 2, 3 };			 // { dg-warning "missing braces" }
int b[2][2] = { { 0, 1 }, { 2, 3 } };
int c[2][2] = { { { 0 }, 1 }, { 2, 3 } };	 // { dg-error "brace-enclosed" }
struct S { char s[6]; int i; };
S d = { "hello", 1 };
S e = { { "hello" }, 1 };
S f = { { { "hello" } }, 1 };			 // { dg-error "brace-enclosed" }
S g = { 'h', 'e', 'l', 'l', 'o', '\0', 1 };	 // { dg-warning "missing braces" }
struct T { wchar_t s[6]; int i; };
T i = { L"hello", 1 };
T j = { { L"hello" }, 1 };
T k = { { { L"hello" } }, 1 };			 // { dg-error "brace-enclosed" }
T l = { L'h', L'e', L'l', L'l', L'o', L'\0', 1 };// { dg-warning "missing braces" }
