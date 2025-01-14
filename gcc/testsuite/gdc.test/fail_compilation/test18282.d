/* REQUIRED_ARGS: -preview=dip1000
   TEST_OUTPUT:
---
fail_compilation/test18282.d(25): Error: returning scope variable `aa` is not allowed in a `@safe` function
fail_compilation/test18282.d(34): Error: escaping a reference to local variable `i` by copying `& i` into allocated memory is not allowed in a `@safe` function
fail_compilation/test18282.d(35): Error: escaping a reference to local variable `i` by copying `& i` into allocated memory is not allowed in a `@safe` function
fail_compilation/test18282.d(36): Error: returning scope variable `staa` is not allowed in a `@safe` function
fail_compilation/test18282.d(44): Error: escaping a reference to local variable `i` by copying `S2000(& i)` into allocated memory is not allowed in a `@safe` function
fail_compilation/test18282.d(53): Error: escaping a reference to local variable `i` by copying `& i` into allocated memory is not allowed in a `@safe` function
fail_compilation/test18282.d(53): Error: escaping a reference to local variable `c` by copying `& c` into allocated memory is not allowed in a `@safe` function
---
 */

// https://issues.dlang.org/show_bug.cgi?id=18282

string* f() @safe
{
    scope string*[] ls;
    return ls[0];
}

int* g() @safe
{
    scope int*[3] aa;
    return aa[0];
}

@safe:

auto bar1()
{
    int i = void;
    int*[1] staa = [ &i ];
    auto    dyna = [ &i ];
    int*[ ] dynb = [ &i ];
    return staa[0];
}

struct S2000 { int* p; }

S2000 bar2()
{
    int i;
    S2000[] arr = [ S2000(&i) ];
    return arr[0];
}


void bar3()
{
    int i;
    char c;
    char*[int*] aa = [ &i : &c ];
}


/******************************
TEST_OUTPUT:
---
fail_compilation/test18282.d(1007): Error: escaping a reference to local variable `foo` by copying `& foo` into allocated memory is not allowed in a `@safe` function
fail_compilation/test18282.d(1008): Error: escaping a reference to local variable `foo` by copying `& foo` into allocated memory is not allowed in a `@safe` function
fail_compilation/test18282.d(1009): Error: escaping a reference to local variable `foo` by copying `& foo` into allocated memory is not allowed in a `@safe` function
fail_compilation/test18282.d(1016): Error: escaping a reference to parameter `this` by copying `&this` into allocated memory is not allowed in a `@safe` function
---
*/

#line 1000

// https://issues.dlang.org/show_bug.cgi?id=18282

void test18282() @safe
{
    string foo = "foo";
    scope string*[] ls;
    ls = ls ~ &foo;
    ls = &foo ~ ls;
    ls ~= &foo;
}

struct S
{
    auto fun()
    {
        arr ~= &this;
    }

    S*[] arr;
}
