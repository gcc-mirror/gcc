// Special g++ Options: -Wshadow

int
main(int i) {
  for(int i=1; i < 3; i++);	// WARNING - shadows parm
  for(int i=1; i < 3; i++);	// WARNING - shadows parm
  for(int j=1; j < 3; j++);
  for(int j=1; j < 3; j++);
}
