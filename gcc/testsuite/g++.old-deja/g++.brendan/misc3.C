// { dg-do assemble  }
// GROUPS passed miscellaneous-bugs
// The compiler should not error about taking the addr of main in this example.
class fred {
private:
  void main () {
  }
public:
  fred ( ) {
    &fred::main;
  }
};
