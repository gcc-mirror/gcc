// PR c++/101460
// { dg-do compile { target c++11 } }

template<bool> struct enable_if { };
template<> struct enable_if<true> { using type = void; };

template<bool B>
using enable_if_t = typename enable_if<B>::type;

struct tuple { };
struct pair { };

template<unsigned N> enable_if_t<N == 1> get(tuple&) { }       // { dg-bogus "candidate" }
template<unsigned N> enable_if_t<N == 1> get(const tuple&) { } // { dg-bogus "candidate" }
template<unsigned N> enable_if_t<N == 1> get(pair&) { }	       // { dg-bogus "candidate" }
template<unsigned N> enable_if_t<N == 1> get(const pair&) { }  // { dg-bogus "candidate" }

template<int N>
constexpr unsigned
frob()
{
  static_assert(N == 1, "user-friendly diagnostic"); // { dg-error "user-friendly" }
  // dg-message { "-1 == 1" "" { target *-*-* } .-1 }

  // narrowing check, reject negative values
  return unsigned{N};		// { dg-prune-output "narrowing" }
} // { dg-prune-output "flows off the end" }
// { dg-prune-output "not a return-statement" }

template<int N> void get_n(tuple& t) { get<frob<N>()>(t); } // { dg-error "" }

int main()
{
  tuple t;
  get_n<-1>(t);
}
