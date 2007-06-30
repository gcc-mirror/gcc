#pragma interface
namespace N {
        typedef int A;
}
inline void g ( ) {
        static N :: A a = 0;
        a = a;
}
