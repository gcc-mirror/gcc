// { dg-do assemble  }
// Test that inheriting from a type typedefed to itself works.

typedef struct class1 {
    class1& operator=(const class1&);
} class1;

class class2 : public class1 { };
