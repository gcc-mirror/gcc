// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi !bad }
// Test for determining various kinds of entities being marked TU-local

export module bad;
import "internal-4_a.H";


// A type, function variable, or template with internal linkage
namespace {
  struct internal_t { int m; };
  enum internal_e {};
  void internal_f() {}
  int internal_v;
  template <typename T> void internal_x() {}
  namespace internal_ns {}
}

inline void expose_type() {  // { dg-error "exposes TU-local entity" }
  internal_t x;
}
inline void expose_func() {  // { dg-error "exposes TU-local entity" }
  internal_f();
}
inline void expose_var() {  // { dg-error "exposes TU-local entity" }
  int* p = &internal_v;
}
inline void expose_tmpl() {  // { dg-error "exposes TU-local entity" }
  internal_x<int>();
}
inline void expose_header_decl() {  // { dg-error "exposes TU-local entity" }
  header_f();
}

// We additionally consider a namespace with internal linkage as TU-local
namespace expose_ns = internal_ns;  // { dg-error "exposes TU-local entity" }

// But we don't consider a weakref as being TU-local, despite being
// marked static; this is to support uses of weakrefs in header files
// (such as via the standard library).
static void weakref() __attribute__((weakref("target")));
inline void expose_weakref() {
  weakref();
}


// Does not have a name with linkage and is declared, or introduced by
// a lambda-expression, within the definition of a TU-local entity
static auto get_local_ok() {
  return 0;
}
static auto get_local_type() {
  struct no_linkage {};
  return no_linkage();
}
static auto get_local_lambda() {
  return []{};
}
using T = decltype(get_local_ok());  // OK
using U = decltype(get_local_type());  // { dg-error "exposes TU-local entity" }
using V = decltype(get_local_lambda());  // { dg-error "exposes TU-local entity" }

static auto internal_lambda = []{ internal_f(); };  // OK
auto expose_lambda = internal_lambda;  // { dg-error "exposes TU-local entity" }

int not_in_tu_local
  = ([]{ internal_f(); }(),  // { dg-error "exposes TU-local entity" }
     0);


// A type with no name that is defined outside a class-specifier, function
// body, or initializer

struct {} no_name;  // { dg-error "exposes TU-local entity" }
enum {} e;  // { dg-error "exposes TU-local entity" }
using not_an_initializer = class {};  // { dg-error "exposes TU-local entity" }

class in_class_specifier { struct {} x; };  // OK
void in_function_body() { struct {} x; }  // OK
auto in_initializer = []{};  // OK

#if __cplusplus >= 202002L
decltype([]{}) d_lambda;  // { dg-error "exposes TU-local entity" "" { target c++20 } }
using alias_lambda = decltype([]{});  // { dg-error "exposes TU-local entity" "" { target c++20 } }

template <typename T>
concept in_constraint_expression = requires {
  // Strictly by the standard this is currently ill-formed
  // (this is a constraint-expression not an initializer)
  // but I don't think that is intended; see CWG2988.
  []{};  // { dg-bogus "exposes TU-local entity" }
};
#endif

// (But consider unnamed types with names for linkage purposes as having names)
typedef struct {} no_name_typedef_t;
no_name_typedef_t linkage_name_struct;  // OK

enum { enum_name } linkage_name_enum;  // OK


// Specialisation of a TU-local template
template <typename T> static void f(T) {}
template <> void f(int) {}  // OK
inline void f_use(int x) {  // { dg-error "exposes TU-local entity" }
  f(x);
}


// Specialisation of a template with any TU-local argument
template <typename T> void g(T) {}
template <> void g(internal_t) { internal_f(); }  // OK
template <> void g(internal_e) { internal_f(); }  // OK
template <> void g(decltype(no_name)) { internal_f(); }  // OK
template <> void g(decltype(get_local_lambda())) { internal_f(); }  // OK

template <auto X> struct h {};
template struct h<&internal_v>;
template <> struct h<&internal_f> { internal_t x; };  // OK
template <> struct h<&internal_t::m> { void foo() { internal_f(); } };  // OK


// TODO: I can't come up with testcases for these that aren't already covered
// by one of the above cases:
//
// - A type with no name introduced by a defining-type-specifier that is
//   used to declare only TU-local entities
// - A specialisation of a template whose (possibly instantiated) declaration
//   is an exposure
