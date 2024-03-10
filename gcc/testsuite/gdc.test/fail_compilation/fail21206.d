// https://issues.dlang.org/show_bug.cgi?id=21206
/* TEST_OUTPUT:
---
fail_compilation/fail21206.d(10): Error: function `fail21206.Obj.toString` cannot return type `string` because its linkage is `extern(C++)`
fail_compilation/fail21206.d(10):        slices are specific to D and do not have a counterpart representation in C++
---
*/
extern(C++) struct Obj
{
    string toString()
    {
        return "ret";
    }
}
