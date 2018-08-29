// PR c++/64848
// { dg-do compile { target c++11 } }

template<typename Signature>
struct function;

template<typename R, typename... Args>
struct function<R (Args...)>
{
  template<typename F>
  function(const F&) { }
};

template<typename T>
class A
{
  T someVar;
};

template<typename T>
class B
{
  int x;

  function<A<double>(A<int>&)> someLambda = [&](A<int>& aInt){
    int xVar = x;
    A<double> aRet;
    return aRet;
  };
};

B<int> a;
