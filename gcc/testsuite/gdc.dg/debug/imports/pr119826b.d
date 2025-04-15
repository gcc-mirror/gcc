module imports.pr119826b;

import pr119826 : t119826;

class C119826
{
    enum E119826 { Evalue }
    const E119826 em = void;
}

void f119826(C119826 c)
{
    t119826(c.em);
}
