// https://issues.dlang.org/show_bug.cgi?id=22646

/*
TEST_OUTPUT:
---
true
true
false
false
---
*/

static template Bug(string name)
{
    enum bool ok = name.length < 3 || name[0..3] != "pad";
}

pragma(msg, Bug!"x".ok);
pragma(msg, Bug!"foo".ok);
pragma(msg, Bug!"pad".ok);
pragma(msg, Bug!"pad123".ok);
