// Origin: Alexander Schiemann (aschiem@count.math.uni-sb.de)

int i;

struct B{};

struct A{

  static void* operator new(unsigned int)
  {return &i;}

  inline static void operator delete(void*p); 

  static void operator delete(void*, const B&){} 

};


inline void A::operator delete(void*p)
{A::operator delete(p,B());}


int main()
{A *ap=new A;
delete ap;}


