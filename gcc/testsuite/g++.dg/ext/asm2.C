// Bug: in a template, we forgot that this was a simple asm, and decided
// that %edi was a malformed operand specifier.

template <class T> class  I {
public:
 void f() { asm ("# mov %edi, %esi" ); }
};

int main () {
  I<int> x;
  x.f();
}
