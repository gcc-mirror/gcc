// PR c++/21619
// { dg-options "" }
int f[__builtin_constant_p(&"Hello"[0])?1:-1];
