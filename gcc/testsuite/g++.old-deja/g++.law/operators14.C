// { dg-do assemble  }
// GROUPS passed operators
void foo (int * a, int * b, int * c) {}

int main() {
	int a,b,c;
	foo (&a, &b, &c);
	(a = b) = c;
}

