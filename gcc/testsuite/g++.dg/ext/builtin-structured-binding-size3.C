// { dg-do compile { target c++11 } }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}

struct A { int a, b, c, d, e; };
struct B {};
struct C { int a, b; };
typedef float V [[gnu::vector_size (16 * sizeof (float))]];
template <>
struct std::tuple_size <C> { static constexpr int value = 42; };

int a = __builtin_structured_binding_size (const A &);		// { dg-error "'__builtin_structured_binding_size' argument 'const A\\\&' is a reference" }
int b = __builtin_structured_binding_size (B &);		// { dg-error "'__builtin_structured_binding_size' argument 'B\\\&' is a reference" }
int c = __builtin_structured_binding_size (C &);		// { dg-error "'__builtin_structured_binding_size' argument 'C\\\&' is a reference" }
int d = __builtin_structured_binding_size (const A (&)[17]);	// { dg-error "'__builtin_structured_binding_size' argument 'const A \\\(\\\&\\\)\\\[17\\\]' is a reference" }
int e = __builtin_structured_binding_size (C (&)[6]);		// { dg-error "'__builtin_structured_binding_size' argument 'C \\\(\\\&\\\)\\\[6\\\]' is a reference" }
int f = __builtin_structured_binding_size (_Complex double &);	// { dg-error "'__builtin_structured_binding_size' argument '__complex__ double\\\&' is a reference" }
int g = __builtin_structured_binding_size (const V &);		// { dg-error "'__builtin_structured_binding_size' argument 'const V\\\&'\[^\n\r]* is a reference" }
int h = __builtin_structured_binding_size (float [[gnu::vector_size (8 * sizeof (float))]] &);	// { dg-error "'__builtin_structured_binding_size' argument '__vector\\\(8\\\) float\\\&' is a reference" }
int i = __builtin_structured_binding_size (A &&);		// { dg-error "'__builtin_structured_binding_size' argument 'A\\\&\\\&' is a reference" }
int j = __builtin_structured_binding_size (B &&);		// { dg-error "'__builtin_structured_binding_size' argument 'B\\\&\\\&' is a reference" }
int k = __builtin_structured_binding_size (C &&);		// { dg-error "'__builtin_structured_binding_size' argument 'C\\\&\\\&' is a reference" }
int l = __builtin_structured_binding_size (A (&&)[17]);		// { dg-error "'__builtin_structured_binding_size' argument 'A \\\(\\\&\\\&\\\)\\\[17\\\]' is a reference" }
int m = __builtin_structured_binding_size (C (&&)[6]);		// { dg-error "'__builtin_structured_binding_size' argument 'C \\\(\\\&\\\&\\\)\\\[6\\\]' is a reference" }
int n = __builtin_structured_binding_size (_Complex double &&);	// { dg-error "'__builtin_structured_binding_size' argument '__complex__ double\\\&\\\&' is a reference" }
int o = __builtin_structured_binding_size (V &&);		// { dg-error "'__builtin_structured_binding_size' argument 'V\\\&\\\&'\[^\n\r]* is a reference" }

template <typename A, typename B, typename C, typename D,
	  typename E, typename F, typename G, typename H>
void
foo ()
{
  int a = __builtin_structured_binding_size (A);		// { dg-error "'__builtin_structured_binding_size' argument 'A\\\&\\\&?' is a reference" }
  int b = __builtin_structured_binding_size (B);		// { dg-error "'__builtin_structured_binding_size' argument '(const )?B\\\&\\\&?' is a reference" }
  int c = __builtin_structured_binding_size (C);		// { dg-error "'__builtin_structured_binding_size' argument 'C\\\&\\\&?' is a reference" }
  int d = __builtin_structured_binding_size (D);		// { dg-error "'__builtin_structured_binding_size' argument 'A \\\(\\\&\\\&?\\\)\\\[17\\\]' is a reference" }
  int e = __builtin_structured_binding_size (E);		// { dg-error "'__builtin_structured_binding_size' argument 'C \\\(\\\&\\\&?\\\)\\\[6\\\]' is a reference" }
  int f = __builtin_structured_binding_size (F);		// { dg-error "'__builtin_structured_binding_size' argument '(const )?__complex__ float\\\&\\\&?' is a reference" }
  int g = __builtin_structured_binding_size (G);		// { dg-error "'__builtin_structured_binding_size' argument '__vector\\\(16\\\) float\\\&\\\&?' is a reference" }
  int h = __builtin_structured_binding_size (H);		// { dg-error "'__builtin_structured_binding_size' argument '__vector\\\(8\\\) float\\\&\\\&?' is a reference" }
}

void
bar ()
{
  foo <A &, const B &, C &, A (&)[17], C (&)[6], const _Complex float &, V &, float [[gnu::vector_size (8 * sizeof (float))]] &> ();
  foo <A &&, B &&, C &&, A (&&)[17], C (&&)[6], _Complex float &&, V &&, float [[gnu::vector_size (8 * sizeof (float))]] &> ();
}
