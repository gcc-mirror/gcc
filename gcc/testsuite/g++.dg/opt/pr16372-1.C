// PR tree-optimization/16372
// { dg-do run }
// { dg-options "-O1" }

extern "C" void abort();

enum number {ZERO, ONE, TWO, THREE, FOUR, FIVE};

int main() {
  number n = FIVE; 
 
  if((n == ONE) || (n == TWO) || (n == THREE)) { 
    abort ();
  } 
  return 0;
}

