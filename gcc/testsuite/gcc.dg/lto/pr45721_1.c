static void bar(void) __attribute__ ((weakref("baz")));
void *x = (void *)bar;
