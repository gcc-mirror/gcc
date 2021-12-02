// REQUIRED_ARGS: -preview=dip1000 -preview=fieldwise
// EXTRA_FILES: imports/impfieldwise.d

import imports.impfieldwise;

@safe:

bool test(S s, S t)
{
    return s == t; // comparison can access fields for ==
}
