struct empty { };
struct something {
	int spacer;
	struct empty foo;
	int bar;
};

struct something X = {
	foo: (struct empty) { },
	bar: 1,
};
