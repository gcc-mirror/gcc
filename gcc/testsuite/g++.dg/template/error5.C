template <typename T>
struct X<T*> { // { dg-error "not a class template" }
   typedef int Y;
};

extern struct Z<int> s; // { dg-error "not a class template" }
