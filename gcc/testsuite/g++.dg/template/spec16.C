// { dg-do compile }
// Contributed by Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// PR c++/14497: Reject specialization without template headers

template <int N>  
struct A { 
  template<int M> void B () ; 
}; 

void A<0>::B<0>() {    // { dg-error "explicit specialization" }
} 
