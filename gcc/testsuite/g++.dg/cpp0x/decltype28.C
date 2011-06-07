// PR c++/44175
// { dg-options -std=c++0x }

template <bool, class T> struct enable_if { };
template <class T> struct enable_if <true, T> { typedef T type; };

template <class F, int N>
void ft (F f, typename enable_if<N!=0, int>::type) {}

template< class F, int N >
decltype(ft<F, N-1> (F(), 0))
ft (F f, typename enable_if<N==0, int>::type) {} // { dg-error "depth" }

int main() {
  ft<struct a*, 2> (0, 0);	// { dg-message "from here" }
}
