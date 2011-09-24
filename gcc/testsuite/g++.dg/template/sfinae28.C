// Origin: PR c++/44267

struct B {};
struct D : B {};
struct VD : virtual B {};

template <class T> T create();

typedef char one[1];
typedef char two[2];

template <class D, class B>
one& f(char (*)[sizeof(static_cast<D>(create<B>()))]);

template <class D, class B>
two& f(...);

int main()
{
   f<D*, int>(0);
   f<D*, B*>(0);
   f<VD*, B*>(0);
   return 0;
}
