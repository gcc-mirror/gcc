// PR c++/32232

template <typename T> struct A;
template <typename T> struct B {};
template <typename T> A<T>& operator<<(A<T>&, const B<T>&);

template <typename T>
struct A 
{
  A<T>& operator<<(A<T>& (*)(A<T>&)); // { dg-message "candidate" }
};

template <typename T> A<T>& foo(A<T>&);
extern A<char> c;

int main () {
  c << (1, foo); // { dg-error "no match" }
}
