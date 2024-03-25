extern char foo[], bar[];
void f (void) { __builtin_memcpy (foo, bar, 7); }
