// { dg-do assemble  }
// GROUPS passed visibility
class A {
public:
        void aMethod(void) {};// { dg-message "" } .*
};

class AA : A { };

class B {
public:
        void thisMethod() {
                AA ana;
                ana.aMethod();// { dg-error "" } .*
        }
};
