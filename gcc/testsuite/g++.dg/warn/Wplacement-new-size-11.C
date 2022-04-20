// PR c++/100370
// { dg-do compile { target c++11 } }

using size_t = decltype(sizeof(1));
inline void *operator new (size_t s, void *p) { return p; }

int main()
{
  struct s1 { int iv[4]; };
  struct s2 { union { char* cp; int* ip; }; };

  s2 b;
  b.ip=new int[8];
  new (b.ip+4) s1;		// { dg-bogus "-Wplacement-new" }
}
