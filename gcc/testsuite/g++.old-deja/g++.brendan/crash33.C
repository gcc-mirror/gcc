// Build don't link: 
// GROUPS passed old-abort
extern void foo(void *);
int
main() {
	foo((struct bar *)0);
}
