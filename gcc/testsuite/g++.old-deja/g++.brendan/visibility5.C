// Build don't link: 
// GROUPS passed visibility
template<int K> class B;
template<int K> class A {int n; friend class B<K>;};
template<int K> class B {public: void method(A<K>) const;};
template<int K> void B<K>::method(A<K> X) const {X.n=0;}
typedef B<2> B2;
 
