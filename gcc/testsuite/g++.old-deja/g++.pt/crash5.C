// Build don't link:

template <class T, int i>
struct K {
	void f();
};

template <class T>
void
K<T, i>::f() // ERROR - i has not been declared
{ 
}
