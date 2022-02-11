// EXTRA_FILES: imports/test72a.d imports/test72b.d imports/test72c.d
module test72;

import imports.test72a, imports.test72c;

void bar()
{
    foo();
}
