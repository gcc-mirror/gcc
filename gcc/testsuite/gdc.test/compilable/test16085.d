// REQUIRED_ARGS: -de
// PERMUTE_ARGS:
import imports.imp16085;

void test()
{
    S s;
    assert(s.functionAndFunction() == Pass());
    assert(s.staticFunctionAndFunction() == Pass());
    // assert(S.staticFunctionAndFunction() == Pass()); // erroneous not accessible error
    assert(s.functionAndTemplate() == Pass());
    assert(s.templateAndTemplate() == Pass());
}
