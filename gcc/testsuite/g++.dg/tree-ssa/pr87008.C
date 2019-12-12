/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern void dontcallthis();

struct A { long a, b; };
struct B : A {};
template<class T>void cp(T&a,T const&b){a=b;}
long f(B x){
  B y; cp<A>(y,x);
  B z; cp<A>(z,x);
  if (y.a - z.a)
    dontcallthis ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "dontcallthis" "optimized" } } */
