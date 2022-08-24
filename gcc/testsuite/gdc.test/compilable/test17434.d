// https://issues.dlang.org/show_bug.cgi?id=17434

// EXTRA_FILES: test17434a.d imports/imp17434a.d imports/imp17434b.d
module test17434;

import test17434a;

void main()
{
    imports.imp17434b.testing();
}
