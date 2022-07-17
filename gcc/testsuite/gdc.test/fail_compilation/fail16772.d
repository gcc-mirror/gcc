// https://issues.dlang.org/show_bug.cgi?id=16772
/* TEST_OUTPUT:
---
fail_compilation/fail16772.d(7): Error: function `fail16772.ice16772` cannot return type `ubyte[]` because its linkage is `extern(C++)`
---
*/
extern(C++) ubyte[] ice16772() { return []; }
