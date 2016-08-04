// pr c++/15049
// Origin: Matt Austern <austern@apple.com>
// Test that we can declare a global variable whose type is anonymous.

// { dg-do compile }

enum { a = 3 } x; // { dg-warning "unnamed type" "" { target { ! c++11 } } }
