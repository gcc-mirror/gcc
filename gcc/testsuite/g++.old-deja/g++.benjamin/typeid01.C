// 980617 bkoz
// typeid for local types
// typeid bool vs int and enum vs int

#include <typeinfo>
#ifdef DEBUG_ASSERT
#include <assert.h>
#endif

// 4: local class in non-main function

void test1 (void) {
  bool class_p = false;
  class X2 { 
  private:
    unsigned int counter;
  public:
    X2 (unsigned int i = 35): counter(i) {}
    ~X2(){}
    unsigned int ret_counter() {return counter;}
  };
  X2 obj_1;
  class_p = typeid(X2) == typeid(obj_1);
}  

int main ()
{
  // 1: simple
#if 1
  bool enum_p = false;
  enum E { A, B, C };
  enum_p = typeid(A) == typeid(E);
#ifdef DEBUG_ASSERT
  assert (enum_p);
#endif
#endif  

  // 2: complex
#if 0
  bool enum2_p = false;
  bool int_p = false;
  bool bool_p = false;
  enum E2 { A2, B2};
  enum2_p = typeid(A2) == typeid(E2);
  int_p =  typeid(int) == typeid(E2);
  bool_p =  typeid(bool) == typeid(E2);
#ifdef DEBUG_ASSERT
  assert (enum2_p);
  assert (!int_p);
  assert (!bool_p);
#endif
#endif

  // 3: local class
  bool class_p = false;
  class X { 
  private:
    unsigned int counter;
  public:
    X (unsigned int i = 35): counter(i) {}
    ~X(){}
    unsigned int ret_counter() {return counter;}
  };
  X obj_1;
  class_p = typeid(X) == typeid(obj_1);

  // 4: local class in function
  test1();

  return 0;
}
