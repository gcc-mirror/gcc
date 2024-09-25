// PR c++/116798
// { dg-do compile { target c++23 } }

template<typename T>
concept C = requires(this T u,	  // { dg-error "'this' specifier in a requires-expression parameter" }
		     this T v) {  // { dg-error "'this' specifier in a requires-expression parameter" }
    u + v;
};

static_assert(C<int>);
