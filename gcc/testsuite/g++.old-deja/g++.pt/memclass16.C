// Build don't link:

template <class Q>
class A {
public:
 
        typedef enum { X, Y } B;
        template <B c> class Traits{ };
};


template class A<int>;
template class A<double>::Traits<A<double>::X>;
