double bar(void), c;
int foo(void) {
	double a, b;
	int i = bar() + bar();
	a = i; i += 1; a += 0.1; i = c + i;
	return i;
}
