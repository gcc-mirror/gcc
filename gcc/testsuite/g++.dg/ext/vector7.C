// { dg-options "" }
// { dg-do compile }
// PR C++/31721 and PR 14217
// the attribute vector_size would change a reference type into a pointer type which was wrong.

#define vector __attribute__((__vector_size__(16) ))
vector int b;
vector int &a = b;
