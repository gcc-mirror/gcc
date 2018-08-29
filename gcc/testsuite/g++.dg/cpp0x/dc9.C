// PR c++/70468
// { dg-do compile { target c++11 } }
// { dg-additional-options -w }

struct S {}; 

template < typename = S > 
class A 
{
public:
  A () : f0 (), f1 () {}	// { dg-error "" }

private:
  typedef A<> f0; 
  int f1;
};

template < typename = S, typename = S > 
class B
{
}; 

template < typename T1, typename T2 > 
B < T1, T2 > &operator<< (B < T1, T2 >&, const int) 
{
  A<> (); 
}

template 
B < S, S > &operator<< (B < S, S >&, const int);
