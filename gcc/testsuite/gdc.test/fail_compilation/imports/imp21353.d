module imports.imp21353;

struct A { int x; }

struct B { import imports.imp21353 : A; }

private struct P { }
