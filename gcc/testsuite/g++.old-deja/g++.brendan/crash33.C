// { dg-do assemble  }
// GROUPS passed old-abort
extern void foo(void *);
int
main() {
	foo((struct bar *)0);
}
