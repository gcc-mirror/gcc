// Build don't link: 
// GROUPS passed visibility
class A {
public:
        void aMethod(void) {};// ERROR - .*
};

class AA : A { };

class B {
public:
        void thisMethod() {
                AA ana;
                ana.aMethod();// ERROR - .*
        }
};
