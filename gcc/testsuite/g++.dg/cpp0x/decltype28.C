// PR c++/44175
// { dg-options -std=c++11 }

template <bool, class T> struct enable_if { };
template <class T> struct enable_if <true, T> { typedef T type; };

template <class F, int N>
void ft (F f, typename enable_if<N!=0, int>::type) {}

template< class F, int N >
decltype(ft<F, N-1> (F(), 0))	// { dg-error "depth" }
ft (F f, typename enable_if<N==0, int>::type) {}

int main() {
  ft<struct a*, 2> (0, 0);	// { dg-message "from here" }
}
