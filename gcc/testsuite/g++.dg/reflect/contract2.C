// C++26 P3598R0 - CWG 3158 - const-ification of Splice Expressions
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

namespace std
{
  template<typename T>
    struct add_lvalue_reference
    { using type = __add_lvalue_reference (T); };
  template<typename T>
    using add_lvalue_reference_t = typename add_lvalue_reference <T>::type;
}

#define UNCONST(x) const_cast <std::add_lvalue_reference_t <decltype (x)>> (x)

template <typename T>
void
foo (T p)			// { dg-error "increment of read-only location '\\\(const int\\\)p'" }
  pre ((++p, true))
{
}

template <typename T>
void
bar (T p)			// { dg-error "increment of read-only location '\\\(const int\\\)p'" }
  pre ((++[:^^p:], true))
{
}

template <typename T>
void
baz (T p)
  pre ((++UNCONST (p), true))
{
}

int v;				// { dg-error "increment of read-only location '\\\(const int\\\)v'" }
int w;				// { dg-error "increment of read-only location '\\\(const int\\\)w'" }
int x;

template <bool B>
void
corge (int p)			// { dg-error "increment of read-only location '\\\(const int\\\)p'" }
  pre ((++(B ? v : p), true))	// { dg-error "increment of read-only location '\\\(B \\\? \\\(\\\(const int\\\)v\\\) : \\\(\\\(const int\\\)p\\\)\\\)'" }
{
}

template <bool B>
void
garply (int p)
  pre ((++[:B ? ^^w : ^^p:], true))
{
}

template <bool B>
void
waldo (int p)
  pre ((++(B ? UNCONST (x) : UNCONST (p)), true))
{
}

void
qux (int p)
{
  foo (p);			// { dg-message "required from here" }
  bar (p);			// { dg-message "required from here" }
  baz (p);
  corge <true> (p);		// { dg-message "required from here" }
  garply <true> (p);		// { dg-message "required from here" }
  waldo <true> (p);
}
