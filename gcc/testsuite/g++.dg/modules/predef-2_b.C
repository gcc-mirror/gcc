// { dg-additional-options "-fmodules-ts -nostdinc -include [srcdir]/predef-2.h" }
// { dg-additional-files predef-2.h }

// test macro expansion inside forced header

export module bob;
// { dg-module-cmi bob }

export import :part;
