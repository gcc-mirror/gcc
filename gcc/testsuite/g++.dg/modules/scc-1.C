export module foo;

export int bar (int);
export int baz (int = bar (1));
export int bar (int = baz (1));
