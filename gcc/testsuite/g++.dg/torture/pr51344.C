/* { dg-do compile } */
class A;

template <class T>
class B
{
  friend __attribute__((noreturn)) A& operator >>(A& a, B& b)
  {
    return a;
  }
};
