// Bug: g++ doesn't understand constructor syntax for pointers.
// Build don't link:

void f () {
  char * p (0);
}
