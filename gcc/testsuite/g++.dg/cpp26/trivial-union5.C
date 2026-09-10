// P3074R7 - trivial unions (was std::uninitialized<T>)
// P3726R2 - Adjustments to Union Lifetime Rules
// { dg-do compile { target c++26 } }

struct S { int a; };
S a = { 5 };
struct T { constexpr T () : a (42) {} int a; };
struct R { int a; constexpr ~R () {} };
union U { int a; S b[2]; };

consteval { int a = 5; __builtin_start_lifetime (&a); }		// { dg-error "'__builtin_start_lifetime' argument type 'int\\\*' is not a pointer to aggregate type" }
consteval { __builtin_start_lifetime (&a); };			// { dg-error "modification of 'a' from outside current evaluation is not a constant expression" }
consteval { __builtin_start_lifetime (); };			// { dg-error "'__builtin_start_lifetime' needs a single argument" }
consteval { S a = { 6 }; __builtin_start_lifetime (&a, &a); };	// { dg-error "'__builtin_start_lifetime' needs a single argument" }
consteval { __builtin_start_lifetime (42); };			// { dg-error "'__builtin_start_lifetime' argument type 'int' is not pointer type" }
consteval { T t; __builtin_start_lifetime (&t); }		// { dg-error "'__builtin_start_lifetime' argument type 'T\\\*' is not a pointer to aggregate type" }
consteval { R r; __builtin_start_lifetime (&r); }		// { dg-error "'__builtin_start_lifetime' argument type 'R\\\*' is not a pointer to implicit-lifetime type" }
consteval { const U u = { .a = 42 }; __builtin_start_lifetime (&u.a); }	// { dg-error "'__builtin_start_lifetime' argument type 'const int\\\*' is not a pointer to aggregate type" }
consteval { const U u = { .a = 42 }; __builtin_start_lifetime (&u.b); }	// { dg-error "modifying a const object 'u.U::b' is not allowed in a constant expression" }

void
foo ()
{
  S s;
  __builtin_start_lifetime (&s);
  int a = 5;
  __builtin_start_lifetime (&a);		// { dg-error "'__builtin_start_lifetime' argument type 'int\\\*' is not a pointer to aggregate type" }
  __builtin_start_lifetime (&::a);
  __builtin_start_lifetime ();			// { dg-error "'__builtin_start_lifetime' needs a single argument" }
  S b = { 6 };
  __builtin_start_lifetime (&b, &b);		// { dg-error "'__builtin_start_lifetime' needs a single argument" }
  __builtin_start_lifetime (42);		// { dg-error "'__builtin_start_lifetime' argument type 'int' is not pointer type" }
  T t;
  __builtin_start_lifetime (&t);		// { dg-error "'__builtin_start_lifetime' argument type 'T\\\*' is not a pointer to aggregate type" }
  R r;
  __builtin_start_lifetime (&r);		// { dg-error "'__builtin_start_lifetime' argument type 'R\\\*' is not a pointer to implicit-lifetime type" }
}
