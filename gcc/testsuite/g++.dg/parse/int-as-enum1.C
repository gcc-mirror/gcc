// PR c++/4207
// Origin: <wanderer@rsu.ru>
// { dg-do compile }

typedef int A;	// { dg-error "previous" }
enum ::A {};	// { dg-error "typedef-name|expected unqualified-id" }
