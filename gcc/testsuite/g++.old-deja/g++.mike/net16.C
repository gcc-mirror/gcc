// { dg-do run  }
// On an i386, this core dumps because the reg-stack.c code is wrong, and
// pops an fp register that it thinks is not used, but it is.

extern "C" int printf (const char*, ...);
struct S { ~S () { } };
double f (S) { return 5; } 
int main() { S s; double dist = f (s); printf ("%g\n", dist); return 0; }
