// Build don't link: 
// GROUPS passed nested-classes
class X {
public:
  struct M2 { int m; };
  M2 g(int);
};


X::M2 X::g(int i) { X::M2 m2; return m2; }

int main() { }
