// Build don't link: 
// GROUPS passed operators

struct A {
        int x;
};

int operator()(A x,float y) {// ERROR - .*
        return 1;
}

int main() {
        A x;
        x(1.0); // ERROR - no match
}

