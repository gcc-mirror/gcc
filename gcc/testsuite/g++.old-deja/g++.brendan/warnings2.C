// { dg-do assemble  }
// GROUPS passed warnings
class K {
public:
        void f() {
        }; // there should be no warning about this semicolon
};
