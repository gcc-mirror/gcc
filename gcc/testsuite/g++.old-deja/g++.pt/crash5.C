// { dg-do assemble  }

template <class T, int i>
struct K {
	void f();
};

template <class T>
void
K<T, i>::f() // { dg-error "" } i has not been declared
{ 
}
