// { dg-do run  }
// PRMS Id: 6275
// Bug: unification fails for call to find_parameter_in_stack.

#include <stdio.h>
#include <stdlib.h>

const int max_stack_size = 20;

template <class T>
class Stack {
  private:
    T objects[max_stack_size];
    int nobjects;
  public:
    Stack(): nobjects(0) {}
    void push(const T&a) {
        if (nobjects >= max_stack_size) {
            fprintf(stderr,"Stack: overflow\n");
            abort();
          }
        objects[nobjects++] = a;
      }
    T pop() {
        if (!nobjects) {
            fprintf(stderr,"Stack: underflow\n");
            abort();
          }
        nobjects -= 1;
        T result = objects[nobjects];
        return result;
      }
    T top() const {
        if (!nobjects) {
            fprintf(stderr,"Stack: underflow\n");
            abort();
          }
        return objects[nobjects - 1];
      }
    int n() const { return nobjects; }
    T operator[](int i) { return objects[i]; }
};

template <class T>
class Parameter {
    T parameter_;
    int is_set_;
    int overrides_;
  public:
    Parameter(): is_set_(0), overrides_(0) {}
    void set(const T& a) { parameter_ = a; is_set_ = 1; }
    void override(int overrides = 1) { overrides_ = overrides; }
    const T& value() const { return parameter_; }
    int overrides() const { return overrides_; }
    int is_set() const { return is_set_; }
};

template <class T1, class T2>
T2
find_parameter_in_stack(Stack<T1>& stack, Parameter<T2>& (T1::*access)())
{
  T2 result;
  int have_result = 0;
  for (int i=stack.n()-1; i>=0; i--) {
      if ((stack[i].*access)().is_set()) {
          if (!have_result || (stack[i].*access)().overrides()) {
              result = (stack[i].*access)().value();
              have_result = 1;
            }
        }
    }
  return result;
}

class A {
  private:
    Parameter<int> a_;
  public:
    A() { }
    Parameter<int>& a() { return a_; }
};

int
main(int, char**)
{
  Stack<A> A_stack;
  A a1;
  A a2;
  a1.a().set(1);
  a2.a().set(2);
  A_stack.push(a1);
  A_stack.push(a2);

  int val = find_parameter_in_stack(A_stack, &A::a);

  printf("val = %d\n", val);
  if (val != 2)
    return 1;

  A_stack.pop();
  A_stack.pop();

  a1.a().override();

  A_stack.push(a1);
  A_stack.push(a2);

  val = find_parameter_in_stack(A_stack, &A::a);

  printf("val = %d\n", val);
  if (val != 1)
    return 1;

  return 0;
}
