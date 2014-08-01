// PR c++/60361

struct Helper
{
  Helper(int a, void (*pfunc)());
};

template <int I> void function();

const int A = 1;
const int B = 2;

Helper testOk(A, function<A>);
Helper testOk2(int(A), function<B>);
Helper testOk3((int(A)), function<A>);
Helper testFail(int(A), function<A>);
