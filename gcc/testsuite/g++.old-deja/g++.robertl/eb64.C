// { dg-do assemble  }
template <class A>
class B:public A {
  B(){}
};

template <class A>
class C:public B<A> {
  C(){}
};

/*
g++  bugsol.C
bugsol.C:9: Internal compiler error.
bugsol.C:9: Please submit a full bug report to `egcs-bugs@cygnus.com'.

g++ -v
Reading specs from
/home/pierre/local/lib/gcc-lib/i586-pc-linux-gnulibc1/egcs-2.90.16/specs
gcc version egcs-2.90.16 971105 (gcc2-970802 experimental)

egcc compiled with gcc version 2.7.2.1 on debian 1.3.1

*/

