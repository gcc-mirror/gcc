// PR c++/39786

namespace A {
    char (*f(char *p))[13] { return 0; }
}

namespace B {
    namespace C {
        char (*f(int p))[42] { return 0; }
    }
    using namespace C;
}

using namespace B;
using namespace A;

char x[sizeof *::f(0) == 42 ? 1 : -1];
