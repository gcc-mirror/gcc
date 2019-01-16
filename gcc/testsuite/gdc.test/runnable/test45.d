// EXTRA_SOURCES: imports/test45a.d imports/test45b.d
// PERMUTE_ARGS:

import imports.test45a;
import imports.test45b;

alias int function() fp1;
alias int function(int) fp2;

void main()
{
    auto i = foo();
    assert(i == 1);
    i = foo(1);
    assert(i == 2);
    i = foo;
    assert(i == 1);

    fp1 fp = &foo;
    i = (*fp)();
    assert(i == 1);

    fp2 fpi = &foo;
    i = (*fpi)(1);
    assert(i == 2);

    i = bar(1);
    assert(i == 3);
    i = bar(1, 2);
    assert(i == 4);
}
