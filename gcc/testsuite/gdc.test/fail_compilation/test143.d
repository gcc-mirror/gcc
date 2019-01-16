// REQUIRED_ARGS: -de
module test143; // Bugzilla 143

import imports.test143;

void bar(int)
{
}

void foo()
{
    bar(x);
}
