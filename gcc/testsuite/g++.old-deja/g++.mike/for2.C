// { dg-do assemble  }

void foo() {
  for (class C {};;)
    ;
  C c;		// { dg-error "declared" "decl" } 
}

void bar() {
  for (enum E {num};;)
    ;
  E e;		// { dg-error "declared" "decl" } 
}

void bee () {
  int i = 0;
  for (int fun() = 0; i != 2; ++i) {	// { dg-warning "extern" "extern" }
  // { dg-error "initialized" "init" { target *-*-* } .-1 }
  }
}
