// PR c++/69658
// { dg-do compile }

struct S { S (int); };
struct T { char n[6]; S s; };
T t[1] = { { "foo", 1 } };	// { dg-bogus "C99 designator" }
