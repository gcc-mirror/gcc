/* { dg-do run { target c++11 } } */
/* There is a devirtualizable call. In PR60306 we deduced wrong target to cxa_pure_virtual.
   For gcc 4.10 we temporarily disable the devirtualization.  */
/* { dg-options "-O3"  } */

#include <vector>

using std::vector;

class Object 
{
public:

  virtual Object* clone() const =0;

  virtual int type() const {return 0;}

  Object& operator=(const Object&) {return *this;}

  Object() {}
  Object(const Object&) {}
  virtual ~Object() {}
};

Object* f(const Object&o)
{
  return o.clone();
}

template<typename T>
class Box: public Object, public T
{
public:
  Box<T>* clone() const {return new Box<T>(*this);}

  Box<T>& operator=(const Box<T>& t)
  {
    T::operator=(t);
    return *this;
  }

  Box<T>& operator=(const T& t)
  {
    T::operator=(t);
    return *this;
  }

  Box() = default;
  Box(const Box<T>&) = default;
  explicit Box(const T& t):T(t) {}
};

template <typename T>
using Vector = Box<vector<T>>;

typedef Vector<int> OVector;

OVector edges_connecting_to_node(int n)
{
  OVector branch_list_;
  for(int i=0;i<n;i++)
    branch_list_.push_back(i);

  return branch_list_;
}

int main(int argc,char* argv[])
{ 
  for(int n=0; n < argc; n++)
  {
    auto x = edges_connecting_to_node(1);
    x.clone();
    f(x);
  }
}
