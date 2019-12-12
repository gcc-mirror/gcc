/* TEST_OUTPUT:
---
fail_compilation/diag16976.d(28): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(29): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(30): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(31): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(32): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(33): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(34): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(35): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(36): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(37): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(38): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(39): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(40): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(41): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(42): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(43): Error: foreach: key cannot be of non-integral type `float`
---
*/

void main()
{
    int[]  dyn = [1,2,3,4,5];
    int[5] sta = [1,2,3,4,5];
    char[]  str = ['1','2','3','4','5'];
    char[5] chr = ['1','2','3','4','5'];
    foreach(float f, i; dyn) {}
    foreach(float f, i; sta) {}
    foreach(float f, i; str) {}
    foreach(float f, i; chr) {}
    foreach(float f, dchar i; dyn) {}
    foreach(float f, dchar i; sta) {}
    foreach(float f, dchar i; str) {}
    foreach(float f, dchar i; chr) {}
    foreach_reverse(float f, i; dyn) {}
    foreach_reverse(float f, i; sta) {}
    foreach_reverse(float f, i; str) {}
    foreach_reverse(float f, i; chr) {}
    foreach_reverse(float f, dchar i; dyn) {}
    foreach_reverse(float f, dchar i; sta) {}
    foreach_reverse(float f, dchar i; str) {}
    foreach_reverse(float f, dchar i; chr) {}
}
