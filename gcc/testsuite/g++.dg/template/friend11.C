/* PR c++/53 */
/* { dg-do compile } */

template <class T> 
struct A {
  template <class U> class B;

  //  Did not compile with gcc-2.95.2 (linux i686)  :-( 
  template <class S> template <class U> friend class A<S>::B; 
};

template <class S> template <class U> class A<S>::B {
}; 

int main(){
  A<double>::B<double>  ab;
  return 0; 
}
