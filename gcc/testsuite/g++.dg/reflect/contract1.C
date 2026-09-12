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

int v;

void
foo (int p)
  pre ((++p, true))		// { dg-error "increment of read-only location '\\\(const int\\\)p'" }
  pre ((++[:^^p:], true))	// { dg-error "increment of read-only location '\\\(const int\\\)p'" }
  pre ((++UNCONST (p), true))
{
}

void
bar ()
  pre ((++v, true))		// { dg-error "increment of read-only location '\\\(const int\\\)v'" }
  pre ((++[:^^v:], true))	// { dg-error "increment of read-only location '\\\(const int\\\)v'" }
  pre ((++UNCONST (v), true))
{
}
