// Test that inheriting from a type typedefed to itself works.
// Build don't link:

typedef struct class1 {
    class1& operator=(const class1&);
} class1;

class class2 : public class1 { };
