// Build don't link:

// Testing overloading of function argument involving template template
// parameters

// Reported by Thomus Kunert <kunert@physik.tu-dresden.de>

template<class A>
class H{};

template <class T>
void f( const T& ){}                      // #1

template< template<class, class> class X, 
	class A, class B>
void f( const X<A,B> & x )                // #2
{}

int main()
{
    H<int> h;
    f(h);                                 // #3
}
