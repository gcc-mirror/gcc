// { dg-do assemble  }
// GROUPS passed operators

struct A {
        int x;
};

int operator()(A x,float y) {// { dg-error "" } .*
        return 1;
}

int main() {
        A x;
        x(1.0); // { dg-error "" } no match
}

