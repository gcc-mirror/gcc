// Build don't link:

void foo() {
  for (class C {};;)
    ;
  C c;		// ERROR - 
}

void bar() {
  for (enum E {num};;)
    ;
  E e;		// ERROR - 
}

void bee () {
  int i = 0;
  for (int fun() = 0; i != 2; ++i) {	// ERROR - 
  }
}
