// Build don't link:

template <class T, int i>
struct K {
	void f();
};  // ERROR - forward declaration

template <class T>
void
K<T, i>::f()
{ // ERROR - template parameters
}
