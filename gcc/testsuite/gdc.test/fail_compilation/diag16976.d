/* TEST_OUTPUT:
---
fail_compilation/diag16976.d(44): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(45): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(46): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(47): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(48): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(49): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(50): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(51): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(52): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(53): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(54): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(55): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(56): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(57): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(58): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(59): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(65): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(66): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(67): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(68): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(69): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(70): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(71): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(72): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(73): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(74): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(75): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(76): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(77): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(78): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(79): Error: foreach: key cannot be of non-integral type `float`
fail_compilation/diag16976.d(80): Error: foreach: key cannot be of non-integral type `float`
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

    immutable int[]  idyn = [1,2,3,4,5];
    immutable int[5] ista = [1,2,3,4,5];
    immutable char[]  istr = ['1','2','3','4','5'];
    immutable char[5] ichr = ['1','2','3','4','5'];
    static foreach(float f, i; idyn) {}
    static foreach(float f, i; ista) {}
    static foreach(float f, i; istr) {}
    static foreach(float f, i; ichr) {}
    static foreach(float f, dchar i; idyn) {}
    static foreach(float f, dchar i; ista) {}
    static foreach(float f, dchar i; istr) {}
    static foreach(float f, dchar i; ichr) {}
    static foreach_reverse(float f, i; idyn) {}
    static foreach_reverse(float f, i; ista) {}
    static foreach_reverse(float f, i; istr) {}
    static foreach_reverse(float f, i; ichr) {}
    static foreach_reverse(float f, dchar i; idyn) {}
    static foreach_reverse(float f, dchar i; ista) {}
    static foreach_reverse(float f, dchar i; istr) {}
    static foreach_reverse(float f, dchar i; ichr) {}
}
