template <class T> void f(T);	// { dg-message "void f" }
template <> int f(int);		// { dg-error "does not match" }

