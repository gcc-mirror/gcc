// Build don't link:
// Special g++ Options: 

typedef struct {
} color;

void foo(color) {
}

#ifdef sparc
void f1() asm("foo__FG3$_0");
void f1() { }
void f2() asm("_foo__FG3$_0");
void f2() { }
void f3() asm("__foo__FG3$_0");
void f3() { }
#endif
