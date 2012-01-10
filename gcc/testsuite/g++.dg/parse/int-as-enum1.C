// PR c++/4207
// Origin: <wanderer@rsu.ru>
// { dg-do compile }

typedef int A;	// { dg-message "previous" }
enum ::A {};	// { dg-error "typedef-name|expected unqualified-id" }
