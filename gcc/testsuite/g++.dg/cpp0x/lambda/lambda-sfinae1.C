// PR c++/56039
// { dg-do compile { target c++11 } }

template <bool> struct BoolSink { typedef void type; };

template <typename T, typename U>
struct AddRvalueReferenceImpl { typedef T type; };

template <typename T>
struct AddRvalueReferenceImpl<T, typename BoolSink<false &&
      [] {			// { dg-error "lambda" "" { target c++17_down } }
         extern T &&tref;
      }>::type> {
   typedef T &&type;
};

template <typename T>
struct AddRvalueReference : AddRvalueReferenceImpl<T, void> { };

namespace ImplHelpers {
   template <typename T>
   typename AddRvalueReference<T>::type create(void);
}

template <typename T, typename U, typename ...Args>
struct IsConstructibleImpl { enum { value = 0 }; };

template <typename T, typename ...Args>
struct IsConstructibleImpl<T, typename BoolSink<false &&
      [] { T t( ::ImplHelpers::create<Args>() ...); } // { dg-error "" }
  >::type, Args ...> {
   enum { value = 1 };
};

template <typename T, typename ...Args>
struct IsConstructible : IsConstructibleImpl<T, void, Args ...> { };

struct DestroyMe {
   ~DestroyMe() = delete;
};

static_assert(+IsConstructible<int>::value, "error");
static_assert(!IsConstructible<void>::value, "error");
static_assert(+IsConstructible<int [1]>::value, "error");
static_assert(!IsConstructible<DestroyMe>::value, "error");
static_assert(!IsConstructible<int *, char *>::value, "error");

static_assert(+IsConstructible<int &&, int>::value, "error");
static_assert(!IsConstructible<int &&, int &>::value, "error");
static_assert(+IsConstructible<int &&, int &&>::value, "error");

// { dg-prune-output "expected" }
// { dg-prune-output "does not name a class" }
// { dg-prune-output "static assertion" }
// { dg-prune-output "template argument . is invalid" }
