// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/link11069x.d
// EXTRA_SOURCES: imports/link11069y.d
// EXTRA_SOURCES: imports/link11069z.d

import imports.link11069y;
import imports.link11069z;

void foo()
{
    Vector2 rsm;
    readWriteVariable(rsm);
}

void main()
{
}
