/*
REQUIRED_ARGS: -Ifail_compilation/imports/
EXTRA_FILES: imports/import21508.d
TEST_OUTPUT:
---
fail_compilation/fail21508.d(17): Error: import `fail21508.import21508` is used as a type
---
*/
import import21508;

// import21508 is a private class, code should not compile
// The compiler used to "helpfully" look inside the import,
// bypassing the shadowing that this introduces.

void main ()
{
    auto c = new import21508();
}
