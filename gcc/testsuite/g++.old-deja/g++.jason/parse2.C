// { dg-do assemble  }
// Bug: g++ doesn't understand constructor syntax for pointers.

void f () {
  char * p (0);
}
