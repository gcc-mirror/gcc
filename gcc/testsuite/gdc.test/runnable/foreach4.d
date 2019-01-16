
import core.stdc.stdio;
import std.stdio;

alias bool bit;

/************************************************/

class Foo
{
    uint array[2];

    int opApply(int delegate(ref uint) dg)
    {
        int result;
        for (int i = 0; i < array.length; i++)
        {
            result = dg(array[i]);
            if (result)
                break;
        }
        return result;
    }
}


/**************************************************/

void test1()
{
    Foo a = new Foo();
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (uint u; a)
    {
        i++;
        u++;
    }
    assert(i == 2);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

void test2()
{
    Foo a = new Foo();
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (ref uint u; a)
    {
        i++;
        u++;
    }
    assert(i == 2);
    assert(a.array[0] == 74);
    assert(a.array[1] == 83);
}

/**************************************************/

void test3()
{
    Foo a = new Foo();
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (ref uint u; a)
    {
        i++;
        if (i)
            break;
        u++;
    }
    assert(i == 1);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

void test4()
{
    Foo a = new Foo();
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (ref uint u; a)
    {
        i++;
        if (i == 1)
            continue;
        u++;
    }
    assert(i == 2);
    assert(a.array[0] == 73 && a.array[1] == 83);
}

/**************************************************/

void test5()
{
    Foo a = new Foo();
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

Loop:
    while (1)
    {
        foreach (ref uint u; a)
        {
            i++;
            if (i)
                break Loop;
            u++;
        }
    }
    assert(i == 1);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

void test6()
{
    Foo a = new Foo();
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

Loop:
    while (1)
    {
        foreach (ref uint u; a)
        {
            i++;
            if (i == 1)
                continue Loop;
            u++;
        }
        break;
    }
    assert(i == 3);
    assert(a.array[0] == 74);
    assert(a.array[1] == 83);
}

/**************************************************/

void test7()
{
    Foo a = new Foo();
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (ref uint u; a)
    {
        i++;
        if (i)
            goto Label;
        u++;
    }
    assert(0);
Label:
    assert(i == 1);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

void test8_x(Foo a)
{
    int i;
    foreach (ref uint u; a)
    {
        i++;
        if (i)
            return;
        u++;
    }
}

void test8()
{
    Foo a = new Foo();
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    test8_x(a);
    assert(i == 0);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

int test9_x(Foo a)
{
    int i;
    foreach (ref uint u; a)
    {
        i++;
        if (i)
            return 67;
        u++;
    }
    return 23;
}

void test9()
{
    Foo a = new Foo();
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    i = test9_x(a);
    assert(i == 67);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

int test10_x(Foo a)
{
    int i;
    foreach (ref uint u; a)
    {
        i++;
        if (i)
            return i;
        u++;
    }
    return 23;
}

void test10()
{
    Foo a = new Foo();
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    i = test10_x(a);
    assert(i == 1);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/
/+
the expected output:

1 1 0 0 0 1 1 0 0 0 1 1 0 0 0 1 1 0 0 0 1 1 0 0 0
1
1 1 0 1 0 1 1 0 1 0 1 1 0 1 0 1 1 0 1 0 1 1 1 1 0
1 1 0 1 0 1 1 0 1 0 1 1 0 1 0 1 1 0 1 0 1 1 1 1 0
1 1 0 1 0 1 1 0 1 0 1 1 0 1 0 1 1 0 1 0 1 1 1 1 0
+/

void test11()
{
    bit[25] data;
    int j;

    for (int i = 0; i < 25; i += 5) {
        data[i+0] = data[i+1] = true;
    }

    for (int i = 0; i < 25; i++) {
        printf("%d ", data[i]);
        if ((i % 5) < 2)
            assert(data[i] == true);
        else
            assert(data[i] == false);
    }

    printf("\n%d\n", data[22] = true);
    j = data[22] = true;
    assert(j == true);

    for (int i = 0; i < 25; i += 5) {
        data[i+1] = data[i+3] = true;
        j = i % 5;
        if (j == 0 || j == 1 || j == 3)
            assert(data[i] == true);
        else
            assert(data[i] == false);
    }

    for (int i = 0; i < 25; i++) {
        printf("%d ", data[i]);
    }

    printf("\n");

    int k;
    foreach (bit b; data) {
        printf("%d ", b);
        j = k % 5;
        if (j == 0 || j == 1 || j == 3 || k == 22)
            assert(data[k] == true);
        else
            assert(data[k] == false);
        k++;
    }
    printf("\n");

    foreach (int l, bit b; data) {
        printf("%d ", b);
        j = l % 5;
        if (j == 0 || j == 1 || j == 3 || l == 22)
            assert(data[l] == true);
        else
            assert(data[l] == false);
    }
    printf("\n");
}


/**************************************************/

void test12()
{
    int j;

    j = 0;
    foreach (dchar d; "hello")
    {
        printf("d = x%x\n", d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        j++;
    }
    assert(j == 5);

    j = 0;
    foreach (size_t i, dchar d; "hello")
    {
        printf("i = %d, d = x%x\n", i, d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        assert(i == j);
        j++;
    }
    assert(j == 5);
}

/**************************************************/

void test13()
{
    int j;

    j = 0;
    foreach (wchar d; "hello")
    {
        printf("d = x%x\n", d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        j++;
    }
    assert(j == 5);

    j = 0;
    foreach (size_t i, wchar d; "hello")
    {
        printf("i = %d, d = x%x\n", i, d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        assert(i == j);
        j++;
    }
    assert(j == 5);
}

/**************************************************/

void test14()
{
    int j;

    j = 0;
    foreach (char d; cast(wstring)"hello")
    {
        printf("d = x%x\n", d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        j++;
    }
    assert(j == 5);

    j = 0;
    foreach (size_t i, char d; cast(wstring)"hello")
    {
        printf("i = %d, d = x%x\n", i, d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        assert(i == j);
        j++;
    }
    assert(j == 5);
}

/**************************************************/

void test15()
{
    int j;

    j = 0;
    foreach (dchar d; cast(wstring)"hello")
    {
        printf("d = x%x\n", d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        j++;
    }
    assert(j == 5);

    j = 0;
    foreach (size_t i, dchar d; cast(wstring)"hello")
    {
        printf("i = %d, d = x%x\n", i, d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        assert(i == j);
        j++;
    }
    assert(j == 5);
}

/**************************************************/

void test16()
{
    int j;

    j = 0;
    foreach (char d; cast(dstring)"hello")
    {
        printf("d = x%x\n", d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        j++;
    }
    assert(j == 5);

    j = 0;
    foreach (size_t i, char d; cast(dstring)"hello")
    {
        printf("i = %d, d = x%x\n", i, d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        assert(i == j);
        j++;
    }
    assert(j == 5);
}

/**************************************************/

void test17()
{
    int j;

    j = 0;
    foreach (wchar d; cast(dstring)"hello")
    {
        printf("d = x%x\n", d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        j++;
    }
    assert(j == 5);

    j = 0;
    foreach (size_t i, wchar d; cast(dstring)"hello")
    {
        printf("i = %d, d = x%x\n", i, d);
        if (j == 0) assert(d == 'h');
        if (j == 1) assert(d == 'e');
        if (j == 2) assert(d == 'l');
        if (j == 3) assert(d == 'l');
        if (j == 4) assert(d == 'o');
        assert(i == j);
        j++;
    }
    assert(j == 5);
}

/**************************************************/

void test18()
{
    string a = "\xE2\x89\xA0";      // \u2260 encoded as 3 UTF-8 bytes

    foreach (dchar c; a)
    {
        printf("a[] = %x\n", c);    // prints 'a[] = 2260'
        assert(c == 0x2260);
    }

    dstring b = "\u2260";

    int i;
    foreach (char c; b)
    {
        printf("%x, ", c);      // prints e2, 89, a0
        if (i == 0) assert(c == 0xE2);
        else if (i == 1) assert(c == 0x89);
        else if (i == 2) assert(c == 0xA0);
        else
            assert(0);
        i++;
    }
    printf("\n");
}

/**************************************************/

void test19()
{
    string string = x"F0 9D 83 93";

    int count=0;
    dchar tmp;
    foreach(dchar value ; string){
        tmp=value;
        count++;
    }
    assert(count==1);
    assert(tmp==0x01D0D3);
}

/**************************************************/

struct S20
{
    int opApply(int delegate(ref int i) dg)
    {
       return 0;
    }
}

S20 foo20;

void test20()
{
    label:
    foreach(int i; foo20)
    {
       continue label;
    }
}


/**************************************************/

void foo21(string[] args)
{
    printf("args.length = %d\n", args.length);
    assert(args.length == 3);
    foreach (i, arg; args)
    {
        assert(typeid(typeof(i)) == typeid(size_t));
        assert(typeid(typeof(arg)) == typeid(string));
        writefln("args[%d] = '%s'", i, arg);
    }
    foreach (arg; args)
    {
        assert(typeid(typeof(arg)) == typeid(string));
        writefln("args[] = '%s'", arg);
    }
}

void test21()
{
    string[] args;

    args.length = 3;
    args[0] = "a";
    args[1] = "bc";
    args[2] = "d";
    foo21(args);
}

/**************************************************/

void test22()
{
    int[string] map;

    map["hello"] = 3;
    map["world"] = 4;

    foreach (key, value; map)
    {
        assert(typeid(typeof(key)) == typeid(string));
        assert(typeid(typeof(value)) == typeid(int));
        writefln("map[%s] = %s", key, value);
    }
    foreach (key, int value; map)
    {
        assert(typeid(typeof(key)) == typeid(string));
        assert(typeid(typeof(value)) == typeid(int));
        writefln("map[%s] = %s", key, value);
    }
    foreach (string key, value; map)
    {
        assert(typeid(typeof(key)) == typeid(string));
        assert(typeid(typeof(value)) == typeid(int));
        writefln("map[%s] = %s", key, value);
    }
    foreach (value; map)
    {
        assert(typeid(typeof(value)) == typeid(int));
        writefln("map[] = %s", value);
    }
}

/**************************************************/

class Foo23
{
    int array[2];

    int opApply(int delegate(ref int) dg)
    {
        int result;
        for (int i = 0; i < array.length; i++)
        {
            result = dg(array[i]);
            if (result)
                break;
        }
        return result;
    }

    int opApply(int delegate(ref size_t, ref int) dg)
    {
        int result;
        for (size_t i = 0; i < array.length; i++)
        {
            result = dg(i, array[i]);
            if (result)
                break;
        }
        return result;
    }
}

void test23()
{
    Foo23 a = new Foo23();
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (u; a)
    {
        assert(typeid(typeof(u)) == typeid(int));
        i++;
        u++;
        //writefln("u = %d", u);
        assert((i == 1) ? u == 74 : u == 83);
    }
    assert(i == 2);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);

    foreach (j, u; a)
    {
        assert(typeid(typeof(j)) == typeid(size_t));
        assert(typeid(typeof(u)) == typeid(int));
        i++;
        u++;
        writefln("u = %d", u);
        assert((i == 3) ? u == 74 : u == 83);
        assert(j == i - 3);
    }
    assert(i == 4);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);
}

/**************************************************/

struct Collection24
{
    int opApply(int delegate(ref int) dg){
        return 0;
    }
}

bool foo24()
{
    Collection24 a,b;

    foreach(int x; a){
        foreach(int y; b){
            return false;
        }
    }

    return true;
}

void test24()
{
    assert(foo24() == true);
}

/**************************************************/

void test25()
{
    alias void function(string[string]) FN;
    FN fn = function (string[string] aarray)
    {
        foreach (string s; aarray)
        {
            writeln(s);
            assert(s == "b");
        }
    };
    string[string] aarray;
    aarray["a"] = "b";
    fn(aarray);
}

/**************************************************/

struct Foo26
{
    uint array[2];

    int forward(int delegate(ref uint) dg)
    {
        int result;
        for (int i = 0; i < array.length; i++)
        {
            result = dg(array[i]);
            if (result)
                break;
        }
        return result;
    }

    int forward(int delegate(ref uint) dg, int x) { return 1; }

    int reverse(int delegate(ref uint) dg, int x) { return 1; }

    int reverse(int delegate(ref uint) dg)
    {
        int result;
        foreach_reverse (v; array)
        {
            auto u = v;
            result = dg(u);
            if (result)
                break;
        }
        return result;
    }
}


void test26()
{
    Foo26 a;
    int i;

    a.array[0] = 73;
    a.array[1] = 82;

    foreach (u; &a.forward)
    {
        writeln(u);
        i++;
        u++;
    }
    assert(i == 2);
    assert(a.array[0] == 73);
    assert(a.array[1] == 82);

    foreach (uint u; &a.reverse)
    {
        writeln(u);
    }
}

/**************************************************/

struct S27
{
    int[] a;

    bool empty() { return a.length == 0; }

    void popFront() { a = a[1 .. $]; }
    int front() { return a[0]; }

    void popBack() { a = a[0 .. $ - 1]; }
    ref int back() { return a[$ - 1]; }
}

void test27()
{
    S27 s;
    s.a = [5,6,7];
    string r;

    foreach (e; s)
    {
        printf("%d\n", e);
        r ~= cast(char)(e + '0');
    }
    assert(r == "567");

    r = null;
    foreach_reverse (ref e; s)
    {
        e++;
        printf("%d\n", e);
        r ~= cast(char)(e + '0');
    }
    assert(r == "876");

    r = null;
    foreach (e; s)
    {
        printf("%d\n", e);
        r ~= cast(char)(e + '0');
    }
    assert(r == "678");
}

/**************************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test9();
    test10();
    test11();
    test12();
    test13();
    test14();
    test15();
    test16();
    test17();
    test18();
    test19();
    test20();
    test21();
    test22();
    test23();
    test24();
    test25();
    test26();
    test27();

    printf("Success\n");
    return 0;
}
