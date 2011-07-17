// PR c++/44175
// { dg-options -std=c++0x }

template <bool, class T> struct enable_if { };
template <class T> struct enable_if <true, T> { typedef T type; };

template <int x>
typename enable_if<x==0,int>::type
ft() {}

template<class F, int N>
decltype (ft<F> (F()))
ft() {}				// { dg-error "depth" }

int main() {
    ft<struct a*, 0>();		// { dg-error "no match|wrong number" }
}

// { dg-prune-output "note" }
