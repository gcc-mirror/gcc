// EXTRA_SOURCES: imports/test17181a.d imports/test17181b.d

module test17181;
import imports.test17181a;

int foo()
{
    return imports.test17181a.abc(1);
}

static this() { assert(a == 2); }
void main() {}
