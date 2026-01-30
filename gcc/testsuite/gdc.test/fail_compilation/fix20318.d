/*
TEST_OUTPUT:
---
fail_compilation/fix20318.d(16): Error: cannot implicitly convert expression `c` of type `const(Bar)` to `Bar` because struct `Bar` contains pointers or references
fail_compilation/fix20318.d(20): Error: cannot implicitly convert expression `complex_c` of type `const(ComplexBar)` to `ComplexBar` because struct `ComplexBar` contains pointers or references
---
*/

void main() {
    // This should work - struct without pointers
    const Foo a;
    Foo b = a; // This is okay

    // This should fail with improved diagnostic message
    const Bar c;
    Bar d = c; // Error with improved diagnostic message

    // Test with a more complex struct with nested pointers
    const ComplexBar complex_c;
    ComplexBar complex_d = complex_c; // Give improved error message
}

struct Foo {
    int value;
}

struct Bar {
    void* ptr;
}

// Simple struct without pointers
struct Simple {
    int value;
}

// Struct with a pointer
struct WithPointer {
    int* ptr;
}

// Complex struct that contains another struct with pointers
struct ComplexBar {
    Simple simple;       // This is fine
    int data;            // This is fine
    WithPointer nested;  // This field prevents implicit conversion
}
