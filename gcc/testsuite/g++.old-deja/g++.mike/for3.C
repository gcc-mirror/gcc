// Special g++ Options: -Wshadow

int
main(int i) {			// WARNING - shadowed decl
  for(int i=1; i < 3; i++);	// WARNING - declaration of
  for(int i=1; i < 3; i++);	// WARNING - declaration of
  for(int j=1; j < 3; j++);
  for(int j=1; j < 3; j++);
}
