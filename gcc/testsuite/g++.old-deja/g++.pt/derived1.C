// { dg-do assemble  }

class A
{
public:
  typedef int Info;
};

template <class T>
class B : public A
{
public:
  typedef struct{
    int a; 
    int b;
  } Info;
};

void f()
{
  B<A>::Info ie;
  ie.a=1;
  ie.b=2;
}
