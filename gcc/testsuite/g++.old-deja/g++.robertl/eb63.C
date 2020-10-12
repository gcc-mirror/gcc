// { dg-do compile  }
// { dg-options "-w -fpermissive" }
//This uses GNU extensions, so disable -ansi
#include <stdio.h>
#include <stdlib.h>

class A {
public:
        A(bool b) { abort(); }
        A(int a, bool b) { printf("cool\n"); }
};

main() {
        A* a;

        a = new A[2](1,false); // { dg-error "parenthesized" "" { target c++17_down } }
}
