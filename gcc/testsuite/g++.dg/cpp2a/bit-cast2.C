// { dg-do compile }

struct S { ~S (); int s; };
S s;
struct V;				// { dg-message "forward declaration of 'struct V'" }
extern V v;				// { dg-error "'v' has incomplete type" }
extern V *p;
struct U { int a, b; };
U u;

void
foo (int *q)
{
  __builtin_bit_cast (int, s);		// { dg-error "'__builtin_bit_cast' source type 'S' is not trivially copyable" }
  __builtin_bit_cast (S, 0);		// { dg-error "'__builtin_bit_cast' destination type 'S' is not trivially copyable" }
  __builtin_bit_cast (int &, q);	// { dg-error "'__builtin_bit_cast' destination type 'int&' is not trivially copyable" }
  __builtin_bit_cast (int [1], 0);	// { dg-error "'__builtin_bit_cast' destination type \[^\n\r]* is an array type" }
  __builtin_bit_cast (V, 0);		// { dg-error "invalid use of incomplete type 'struct V'" }
  __builtin_bit_cast (int, v);
  __builtin_bit_cast (int, *p);		// { dg-error "invalid use of incomplete type 'struct V'" }
  __builtin_bit_cast (U, 0);		// { dg-error "'__builtin_bit_cast' source size '\[0-9]*' not equal to destination type size '\[0-9]*'" }
  __builtin_bit_cast (int, u);		// { dg-error "'__builtin_bit_cast' source size '\[0-9]*' not equal to destination type size '\[0-9]*'" }
}

template <int N>
void
bar (int *q)
{
  __builtin_bit_cast (int, s);		// { dg-error "'__builtin_bit_cast' source type 'S' is not trivially copyable" }
  __builtin_bit_cast (S, 0);		// { dg-error "'__builtin_bit_cast' destination type 'S' is not trivially copyable" }
  __builtin_bit_cast (int &, q);	// { dg-error "'__builtin_bit_cast' destination type 'int&' is not trivially copyable" }
  __builtin_bit_cast (int [1], 0);	// { dg-error "'__builtin_bit_cast' destination type \[^\n\r]* is an array type" }
  __builtin_bit_cast (V, 0);		// { dg-error "invalid use of incomplete type 'struct V'" }
  __builtin_bit_cast (int, *p);		// { dg-error "invalid use of incomplete type 'struct V'" }
  __builtin_bit_cast (U, 0);		// { dg-error "'__builtin_bit_cast' source size '\[0-9]*' not equal to destination type size '\[0-9]*'" }
  __builtin_bit_cast (int, u);		// { dg-error "'__builtin_bit_cast' source size '\[0-9]*' not equal to destination type size '\[0-9]*'" }
}

template <typename T1, typename T2, typename T3, typename T4>
void
baz (T3 s, T4 *p, T1 *q)
{
  __builtin_bit_cast (int, s);		// { dg-error "'__builtin_bit_cast' source type 'S' is not trivially copyable" }
  __builtin_bit_cast (T3, 0);		// { dg-error "'__builtin_bit_cast' destination type 'S' is not trivially copyable" }
  __builtin_bit_cast (T1 &, q);		// { dg-error "'__builtin_bit_cast' destination type 'int&' is not trivially copyable" }
  __builtin_bit_cast (T2, 0);		// { dg-error "'__builtin_bit_cast' destination type \[^\n\r]* is an array type" }
  __builtin_bit_cast (T4, 0);		// { dg-error "invalid use of incomplete type 'struct V'" }
  __builtin_bit_cast (int, *p);		// { dg-error "invalid use of incomplete type 'struct V'" }
  __builtin_bit_cast (U, (T1) 0);	// { dg-error "'__builtin_bit_cast' source size '\[0-9]*' not equal to destination type size '\[0-9]*'" }
  __builtin_bit_cast (T1, u);		// { dg-error "'__builtin_bit_cast' source size '\[0-9]*' not equal to destination type size '\[0-9]*'" }
}

void
qux (int *q)
{
  baz <int, int [1], S, V> (s, p, q);
}
