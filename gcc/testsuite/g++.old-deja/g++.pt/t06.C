// Build don't link: 

typedef int I;
int i;

template <class A> class B {
  A a;
 public:
  B(A&aa);
  B();
  ~B();
};

template <class B> class C { public: B b; };

template <class I, class i> class D : I { public: i ii; };

typedef B<int> b_int;
typedef C<int> c_int;      
typedef C<b_int> c_b_int2; 

c_b_int2 x2;
int z;
D<c_b_int2,b_int> d;
int q;
