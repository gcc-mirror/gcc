/* TEST_OUTPUT:
---
fail_compilation/fail22881.d(101): Error: pointer slice `[0..6]` exceeds allocated memory block `[0..5]`
fail_compilation/fail22881.d(102): Error: pointer slice `[0..6]` exceeds allocated memory block `[0..5]`
fail_compilation/fail22881.d(110): Error: pointer slice `[3..5]` exceeds allocated memory block `[0..4]`
fail_compilation/fail22881.d(113):        called from here: `ptr22881()`
fail_compilation/fail22881.d(113):        while evaluating: `static assert(ptr22881())`
fail_compilation/fail22881.d(203): Error: slice `[0..2]` is out of bounds
fail_compilation/fail22881.d(207):        called from here: `null22881()`
fail_compilation/fail22881.d(207):        while evaluating: `static assert(null22881())`
fail_compilation/fail22881.d(305): Error: slice `[2..4]` exceeds array bounds `[0..3]`
fail_compilation/fail22881.d(308):        called from here: `slice22881()`
fail_compilation/fail22881.d(308):        while evaluating: `static assert(slice22881())`
fail_compilation/fail22881.d(401): Error: slice `[0..1]` exceeds array bounds `[0..0]`
fail_compilation/fail22881.d(403): Error: slice `[0..1]` exceeds array bounds `[0..0]`
---
*/
#line 100
// SliceExp: e1.type.ty == pointer
static pstr22881 = "hello".ptr[0 .. 6];
static parr22881 = ['h','e','l','l','o'].ptr[0 .. 6];

bool ptr22881()
{
    char *p1 = new char[4].ptr;
    p1[0 .. 4] = "str\0";
    char *s1 = p1[1 .. 3].ptr;
    char *s2 = s1[1 .. 3].ptr;  // = p1[2 .. 4]
    char *s3 = s2[1 .. 3].ptr;  // = p1[3 .. 5]
    return true;
}
static assert(ptr22881());


#line 200
// SliceExp: e1.op == null
bool null22881()
{
    string[][1] nullexp;
    nullexp[0][0 .. 2] = "st";
    return true;
}
static assert(null22881());

#line 300
// SliceExp: e1.op == slice
bool slice22881()
{
    char[] str = "abcd".dup;
    char[] slice = str[1 .. 4];
    slice[2 .. 4] = "ab";
    return true;
}
static assert(slice22881());

#line 400
// SliceExp: e1.op == arrayLiteral
static arr22881 = [][0 .. 1];
// SliceExp: e1.op == string_
static str22881 = ""[0 .. 1];
