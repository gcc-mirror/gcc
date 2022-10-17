template <class T>
void f(T);

template<> void f(int, ...);	// { dg-error "match" }

template <class T>
void g(T, ...);

template<> void g(int);		// { dg-error "match" }
