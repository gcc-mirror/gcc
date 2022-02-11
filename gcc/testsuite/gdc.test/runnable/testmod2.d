// EXTRA_SOURCES: imports/testmod2a.d

/**********************************/
// https://issues.dlang.org/show_bug.cgi?id=1904

import imports.testmod2a;
void main()
{
    void whatever() {}
    foo!(whatever)();
}

