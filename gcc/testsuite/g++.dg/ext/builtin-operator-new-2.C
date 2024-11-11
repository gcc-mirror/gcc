// { dg-do compile }

#include <new>

void *operator new (std::size_t, float, double);	// { dg-message "selected function declared here" }
void operator delete (void *, float, double);		// { dg-message "selected function declared here" }

void
foo (void *x)
{
  void *a = __builtin_operator_new (32, 1.f, 32.);	// { dg-error "call to '__builtin_operator_new' does not select replaceable global allocation function" }
  __builtin_operator_delete (a, 1.f, 32.);		// { dg-error "call to '__builtin_operator_delete' does not select replaceable global deallocation function" }
  void *b = __builtin_operator_new (32, x);		// { dg-error "call to '__builtin_operator_new' does not select replaceable global allocation function" }
  void *c = __builtin_operator_new (32, 1LL, 1L, 1);	// { dg-error "no matching function for call to 'operator new\\\(int, long long int, long int, int\\\)'" }
  __builtin_operator_delete (c, 1LL, 1L, 1);		// { dg-error "no matching function for call to 'operator delete\\\(void\\\*&, long long int, long int, int\\\)'" }
  void *d = __builtin_operator_new ();			// { dg-error "no matching function for call to 'operator new\\\(\\\)'" }
  __builtin_operator_delete ();				// { dg-error "no matching function for call to 'operator delete\\\(\\\)'" }
}

template <int N>
void
bar (void *x)
{
  void *a = __builtin_operator_new (32, 1.f, 32.);	// { dg-error "call to '__builtin_operator_new' does not select replaceable global allocation function" }
  __builtin_operator_delete (a, 1.f, 32.);		// { dg-error "call to '__builtin_operator_delete' does not select replaceable global deallocation function" }
  void *b = __builtin_operator_new (32, x);		// { dg-error "call to '__builtin_operator_new' does not select replaceable global allocation function" }
  void *c = __builtin_operator_new (32, 1LL, 1L, 1);	// { dg-error "no matching function for call to 'operator new\\\(int, long long int, long int, int\\\)'" }
  __builtin_operator_delete (c, 1LL, 1L, 1);		// { dg-error "no matching function for call to 'operator delete\\\(void\\\*&, long long int, long int, int\\\)'" }
  void *d = __builtin_operator_new ();			// { dg-error "no matching function for call to 'operator new\\\(\\\)'" }
  __builtin_operator_delete ();				// { dg-error "no matching function for call to 'operator delete\\\(\\\)'" }
}

template <typename T, typename U, typename V>
void
baz (void *x, T sz, V v)
{
  U a = __builtin_operator_new (sz, 1.f, 32.);		// { dg-error "call to '__builtin_operator_new' does not select replaceable global allocation function" }
  __builtin_operator_delete (a, 1.f, 32.);		// { dg-error "call to '__builtin_operator_delete' does not select replaceable global deallocation function" }
  U b = __builtin_operator_new (sz, x);			// { dg-error "call to '__builtin_operator_new' does not select replaceable global allocation function" }
  U c = __builtin_operator_new (sz, v, 1L, 1);		// { dg-error "no matching function for call to 'operator new\\\(int&, long long int&, long int, int\\\)'" }
  __builtin_operator_delete (c, v, 1L, 1);		// { dg-error "no matching function for call to 'operator delete\\\(void\\\*&, long long int&, long int, int\\\)'" }
}

void
qux (void *x, void *y)
{
  bar <0> (x);
  baz <int, void *, long long> (y, 32, 1LL);
}
