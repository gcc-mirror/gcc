void bar(void *) __attribute__((__nonnull__));
void *baz(void);
void foo(void) { bar(baz()); }
