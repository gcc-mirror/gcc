// { dg-do assemble  }
// GROUPS passed visibility
class A {
public:
        void aMethod(void) {};// { dg-error "" } .*
};

class AA : A { };

class B {
public:
        void thisMethod() {
                AA ana;
                ana.aMethod();// { dg-error "" } .*
        }
};
