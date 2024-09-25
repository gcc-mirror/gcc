// PR preprocessor/80005
// { dg-do preprocess }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#undef vector
#define vector NOPE
#ifdef __has_include

#if !__has_include (<vector>)
#error "Header 'vector' could not be found"
#endif
#define F(X) __has_include (X)
#if !F (<vector>)
#error "Header 'vector' could not be found" // { dg-error "not be found" }
#endif

#if __has_include ("not an escape! \")	// comment
#endif

#if F ("is an escape \") gibberish ")
#endif

#else
#error "No __has_include"
#endif
