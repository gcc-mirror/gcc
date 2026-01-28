// PR tree-optimization/123849
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }
// { dg-add-options float16 }
// { dg-require-effective-target float16 }

#include <new>

template<typename Derived> class MatrixBase
{
public:
  MatrixBase();
};

template<typename Derived>
class PlainObjectBase : public MatrixBase<Derived>
{
public:
  void *operator new(std::size_t size);
  void operator delete(void * ptr);
};

template<typename _Scalar>
class Matrix
  : public PlainObjectBase<Matrix<_Scalar> >
{
};

template<typename T>
struct resource
{
  T& makeref() const
  {
    T* ret = new T;
    return *ret;
  }
};


using T = Matrix<_Float16>;

void func(resource<T>& A)
{
  (void)A.makeref();
}
