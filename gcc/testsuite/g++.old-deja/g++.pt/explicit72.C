// { dg-do assemble  }
// Contributed by Reid M. Pinchback <reidmp@MIT.EDU>
// Adapted by Alexandre Oliva <oliva@dcc.unicamp.br>
// plain char, signed char and unsigned char are distinct types

template <class X, class Y> struct bug {};
template <class X> struct bug<X,char> { typedef char t; };
template <class X> struct bug<X,unsigned char> { typedef unsigned char t; };
template <class X> struct bug<X,signed char> { typedef signed char t; };
template <class X> struct bug<char,X> { typedef char t; };
template <class X> struct bug<unsigned char,X> { typedef unsigned char t; };
template <class X> struct bug<signed char,X> { typedef signed char t; };

void foo() {
  bug<int,char>::t();
  bug<int,signed char>::t();
  bug<int,unsigned char>::t();
  bug<char,int>::t();
  bug<signed char,int>::t();
  bug<unsigned char,int>::t();
}
