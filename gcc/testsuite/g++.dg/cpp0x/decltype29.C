// PR c++/44175
// { dg-options -std=c++11 }

template <bool, class T> struct enable_if { };
template <class T> struct enable_if <true, T> { typedef T type; };

template <int x>
typename enable_if<x==0,int>::type
ft() {}

template<class F, int N>
decltype (ft<F> (F()))		// { dg-error "depth" }
ft() {}

int main() {
    ft<struct a*, 0>();		// { dg-error "no match|wrong number" }
}

// { dg-prune-output "note" }
