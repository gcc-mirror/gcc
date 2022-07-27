/* TEST_OUTPUT:
---
tuple((Attrib))
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17545

module example;

struct Attrib {}

@Attrib enum TEST = 123;

pragma(msg, __traits(getAttributes,
                     __traits(getMember, example, "TEST")));
