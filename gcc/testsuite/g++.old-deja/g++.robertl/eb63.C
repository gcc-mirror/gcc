//Special g++ Options:
//This uses GNU extensions, so disable -ansi
#include <stdio.h>

class A {
public:
        A(bool b) { abort(); }
        A(int a, bool b) { printf("cool\n"); }
};

main() {
        A* a;
	a = new A[2] = { A(1,false), A(1,false) } ;
        a = new A[2](1,false);
}
