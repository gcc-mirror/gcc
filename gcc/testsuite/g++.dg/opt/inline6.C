// PR c++/13081
// { dg-options "-O2" }
// { dg-final { scan-assembler-not "foo" } }

template<typename T> T foo(T);
 
template<typename T> inline T foo(T t)
{
  return t;
}

void bar (long& l) {
  l = foo(l);
}
