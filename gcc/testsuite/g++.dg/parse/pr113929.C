// PR c++/113929
// { dg-do compile }

template <this int C>		// { dg-error "'this' specifier in template parameter declaration" }
struct S {};
template <int N, this int C>	// { dg-error "'this' specifier in template parameter declaration" }
struct T {};
