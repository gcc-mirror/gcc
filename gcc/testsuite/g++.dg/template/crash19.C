// PR c++/15165

struct S 
{ 
    template <class T> S(const T &e);
};
int operator *(const double, const S &); 
template <class T>
struct X { 
    enum { SIXTY_FOUR=64 }; 
    struct node {
      unsigned char *ptr[sizeof(T)*SIXTY_FOUR]; // { dg-error "" }
        void d() {}
    };
    node *head; 
};
template struct X<int>;
