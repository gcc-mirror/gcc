// Build don't run:

// Based on a testcase by eyal.ben-david@aks.com

// An extern declaration of an undeclared object within a function
// introduces the object into the enclosing namespace [basic.link]/7

namespace {
  void foo() {
    extern int xx;
    xx = 0;
  }
  int xx = 1;
}

int main() {
  xx = 2;
}
