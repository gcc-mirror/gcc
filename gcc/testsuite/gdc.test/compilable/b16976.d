/* REQUIRED_ARGS: -verrors=simple -m64
TEST_OUTPUT:
---
compilable/b16976.d(33): Deprecation: foreach: loop index implicitly converted from `size_t` to `int`
compilable/b16976.d(34): Deprecation: foreach: loop index implicitly converted from `size_t` to `int`
compilable/b16976.d(35): Deprecation: foreach: loop index implicitly converted from `size_t` to `char`
compilable/b16976.d(36): Deprecation: foreach: loop index implicitly converted from `size_t` to `char`
compilable/b16976.d(41): Deprecation: foreach: loop index implicitly converted from `size_t` to `int`
compilable/b16976.d(42): Deprecation: foreach: loop index implicitly converted from `size_t` to `int`
compilable/b16976.d(43): Deprecation: foreach: loop index implicitly converted from `size_t` to `char`
compilable/b16976.d(44): Deprecation: foreach: loop index implicitly converted from `size_t` to `char`
compilable/b16976.d(50): Deprecation: foreach: loop index implicitly converted from `size_t` to `int`
compilable/b16976.d(51): Deprecation: foreach: loop index implicitly converted from `size_t` to `int`
compilable/b16976.d(52): Deprecation: foreach: loop index implicitly converted from `size_t` to `char`
compilable/b16976.d(53): Deprecation: foreach: loop index implicitly converted from `size_t` to `char`
compilable/b16976.d(58): Deprecation: foreach: loop index implicitly converted from `size_t` to `int`
compilable/b16976.d(59): Deprecation: foreach: loop index implicitly converted from `size_t` to `int`
compilable/b16976.d(60): Deprecation: foreach: loop index implicitly converted from `size_t` to `char`
compilable/b16976.d(61): Deprecation: foreach: loop index implicitly converted from `size_t` to `char`
compilable/b16976.d(62): Deprecation: foreach: loop index implicitly converted from `size_t` to `int`
compilable/b16976.d(63): Deprecation: foreach: loop index implicitly converted from `size_t` to `int`
compilable/b16976.d(64): Deprecation: foreach: loop index implicitly converted from `size_t` to `char`
compilable/b16976.d(65): Deprecation: foreach: loop index implicitly converted from `size_t` to `char`
---
*/
void main()
{
    int[]  dyn = [1,2,3,4,5];
    int[5] sta = [1,2,3,4,5];
    char[]  str = ['1','2','3','4','5'];
    char[5] chr = ['1','2','3','4','5'];

    foreach(int i, v; dyn) { }
    foreach_reverse(int i, v; dyn) { }
    foreach(char i, v; dyn) { }
    foreach_reverse(char i, v; dyn) { }
    foreach(int i, v; sta) { }
    foreach_reverse(int i, v; sta) { }
    foreach(char i, v; sta) { }
    foreach_reverse(char i, v; sta) { }
    foreach(int i, v; str) { }
    foreach_reverse(int i, v; str) { }
    foreach(char i, v; str) { }
    foreach_reverse(char i, v; str) { }
    foreach(int i, v; chr) { }
    foreach_reverse(int i, v; chr) { }
    foreach(char i, v; chr) { }
    foreach_reverse(char i, v; chr) { }

    foreach(int i, dchar v; dyn) { }
    foreach_reverse(int i, dchar v; dyn) { }
    foreach(char i, dchar v; dyn) { }
    foreach_reverse(char i, dchar v; dyn) { }
    foreach(int i, dchar v; sta) { }
    foreach_reverse(int i, dchar v; sta) { }
    foreach(char i, dchar v; sta) { }
    foreach_reverse(char i, dchar v; sta) { }
    foreach(int i, dchar v; str) { }
    foreach_reverse(int i, dchar v; str) { }
    foreach(char i, dchar v; str) { }
    foreach_reverse(char i, dchar v; str) { }
    foreach(int i, dchar v; chr) { }
    foreach_reverse(int i, dchar v; chr) { }
    foreach(char i, dchar v; chr) { }
    foreach_reverse(char i, dchar v; chr) { }
}
