struct empty { };
struct something {
	int spacer;
	struct empty foo;
	int bar;
};

struct something X = {
	foo: { },
	bar: 1,
};
