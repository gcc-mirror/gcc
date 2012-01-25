/* { dg-do compile { target { i?86-*-* && ilp32 } } } */
class A;

template <class T>
class B
{
  friend __attribute__((cdecl)) A& operator >>(A& a, B& b)
  {
    return a;
  }
};
