// Build don't link: 
// GROUPS passed operators
struct A {
	int x;
};

int operator()(A x,float y) { // MUST be a member function// ERROR - .*
	return 1;
}

int main() {
	A x;
	x(1.0); // ERROR - no match for call
}
