// { dg-do assemble  }
// { dg-options "-Wshadow" }

int
main(int i) {			// { dg-warning "" } shadowed decl
  for(int i=1; i < 3; i++);	// { dg-warning "" } declaration of
  for(int i=1; i < 3; i++);	// { dg-warning "" } declaration of
  for(int j=1; j < 3; j++);
  for(int j=1; j < 3; j++);
}
