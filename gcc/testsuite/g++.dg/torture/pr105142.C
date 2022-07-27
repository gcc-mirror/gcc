// { dg-do run { target lp64 } }

long int c = 3623214276426624192L;
unsigned short b;
char a = 42;
const long &min(const long &x, const long &y) { return x < y ? x : y; }
__attribute__((noipa)) void test() { b = min(a, min(a, c) + 5713568809962283044L); }
int main() { test(); if (b != 42) __builtin_abort(); }
