// Build don't link: 
// GROUPS passed old-abort
extern void foo(void *);
main() {
	foo((struct bar *)0);
}
