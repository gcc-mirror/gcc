struct s { float f[1]; };
struct s foo();
float bar() { return foo().f[0]; }
