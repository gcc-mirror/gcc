// { dg-do assemble  }

void foo() {
  for (class C {};;)
    ;
  C c;		// { dg-error "" } 
}

void bar() {
  for (enum E {num};;)
    ;
  E e;		// { dg-error "" } 
}

void bee () {
  int i = 0;
  for (int fun() = 0; i != 2; ++i) {	// { dg-error "" } 
  }
}
