// Build don't link: 
// GROUPS passed operators
void foo (int * a, int * b, int * c) {}

main() {
	int a,b,c;
	foo (&a, &b, &c);
	(a = b) = c;
}

