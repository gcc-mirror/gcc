// PR c++/5857
// This testcase failed because during duplicate_decls the type was promoted
// to int.

// { dg-do compile }

typedef char baz;
extern const char foo[];
const baz foo[] = "xyz";
const char bar[] = "abc";
