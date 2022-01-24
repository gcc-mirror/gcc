// https://issues.dlang.org/show_bug.cgi?id=19775
enum x1 = 42;
enum ident(alias a, args...) = mixin(a, args);

//enum x2 = ident!("x1"); //FIXME - empty args
enum x2 = x1;
enum x3 = ident!("x", 2);
enum x4 = ident!('x', "", 3);
enum x5 = ident!("", 'x', 4);

//static assert(x2 == 42);
static assert(x3 == 42);
static assert(x4 == 42);
static assert(x5 == 42);
