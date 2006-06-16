/* { dg-do link } */

extern void link_error ();

void a (int x) {
	if (~ (~x) - x)
		link_error ();
}
void b (int x) {
	if (- (-x) - x)
		link_error ();
}

void c (int x) {
	if (!(- (~x) - x))
		link_error ();
}

void d (int x) {
	if (!(~ (-x) - x))
		link_error ();
}

void e (int x) {
	if (x + ~(x - 1))
		link_error ();
}

void f (int x) {
	if (x + ~(x + (-1)))
		link_error ();
}

int main (int argc, char *argv[]) {
	a(argc);
	b(argc);
	c(argc);
	d(argc);
	e(argc);
	f(argc);
	return 0;
}
