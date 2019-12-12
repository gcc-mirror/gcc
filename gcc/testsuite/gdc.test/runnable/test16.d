// REQUIRED_ARGS:

extern(C) int printf(const char*, ...);

/************************************************/
// These seem to be the original tests for $ (originally 'length').

int x;

int[] bar(int[] a)
{
    x++;
    return a[0 .. $ - 1];
}

void test1()
{
  {
    int[4] foo;

    foo[$ - 2] = 4;
    assert(foo[0] == 0);
    assert(foo[1] == 0);
    assert(foo[2] == 4);
    assert(foo[3] == 0);

    foo[3] = 5;
    assert(foo[$ - 1] == 5);

    x = 0;
    bar(foo)[$ - 3] = 6;
    assert(x == 1);
    assert(foo[0] == 6);

    assert(bar(foo)[$ * 2 - 1 - $] == 4);
    assert(x == 2);

    foo[0 .. $] = 1;
    assert(foo[0] == 1);
    assert(foo[1] == 1);
    assert(foo[2] == 1);
    assert(foo[3] == 1);

    x = 0;
    bar(foo)[1 .. $ * 3 - $ - $] = 2;
    assert(x == 1);
    assert(foo[0] == 1);
    assert(foo[1] == 2);
    assert(foo[2] == 2);
    assert(foo[3] == 1);

    int[] a = new int[3];
    a[0..$] = foo[0..$-1];
    assert(a[0] == 1);
    assert(a[1] == 2);
    assert(a[2] == 2);
    a[] = 4;
    a[0..$] = bar(foo)[0..$];
    assert(x == 2);
    assert(a[0] == 1);
    assert(a[1] == 2);
    assert(a[2] == 2);
  }

  {
    int[4] f; int[] foo = f;

    foo[$ - 2] = 4;
    assert(foo[0] == 0);
    assert(foo[1] == 0);
    assert(foo[2] == 4);
    assert(foo[3] == 0);

    foo[3] = 5;
    assert(foo[$ - 1] == 5);

    x = 0;
    bar(foo)[$ - 3] = 6;
    assert(x == 1);
    assert(foo[0] == 6);

    assert(bar(foo)[$ * 2 - 1 - $] == 4);
    assert(x == 2);

    foo[0 .. $] = 1;
    assert(foo[0] == 1);
    assert(foo[1] == 1);
    assert(foo[2] == 1);
    assert(foo[3] == 1);

    x = 0;
    bar(foo)[1 .. $ * 3 - $ - $] = 2;
    assert(x == 1);
    assert(foo[0] == 1);
    assert(foo[1] == 2);
    assert(foo[2] == 2);
    assert(foo[3] == 1);

    int[] a = new int[3];
    a[0..$] = foo[0..$-1];
    assert(a[0] == 1);
    assert(a[1] == 2);
    assert(a[2] == 2);
    a[] = 4;
    a[0..$] = bar(foo)[0..$];
    assert(x == 2);
    assert(a[0] == 1);
    assert(a[1] == 2);
    assert(a[2] == 2);
  }
}

/************************************************/

struct ICONINFO
{
    bool fIcon;
}

void test2()
{
     ICONINFO info;
     info.fIcon = true;
     assert(info.fIcon == true);
}


/************************************************/

class A3
{
    void foo()
    {
        printf("A.foo \n" );
    }
}

class B3 : A3
{
}

class C3 : B3
{
    override void foo()
    {
        printf("C.foo \n" );
        super.foo();
    }
}

void test3()
{
    C3 c = new C3();
    c.foo();
}


/************************************************/

void test4()
{
    int function (int i) x = function int (int i) { return i * 2; };
    int function (int i) y = function int (int i) { return i / 2; };

    int k;
    k = x(2);
    assert(k == 4);
    k = y(3);
    assert(k == 1);
}


/************************************************/

class Parser
{
    void next(ref int test)
    {
        void work (int input)
        {
            printf("work(%d, %d)\n", input, test);
            test = 2;
        }

        test = 3;
        work(4);
    }
}


void test5()
{
    Parser parser = new Parser();
    int test;

    parser.next (test);
    printf("test %d\n", test);
    assert(test == 2);
}


/************************************************/

void foo6(out int bar)
{
}

void test6()
{
    int bar = 3;
    foo6(bar);
    printf("%d", bar );
    assert(bar == 0);
//    return 0;
}

/************************************************/

void test7()
{
   char ch = ' ';
   char[] u;
   u.length = 3;

   int i = 2;

   printf("a\n");
   u[0..2] = ch;
   printf("b\n");
   u[0..i] = ch;
   printf("c\n");
   assert(u[0] == 0x20);
   assert(u[1] == 0x20);
}


/************************************************/

struct X8
{
    bool flag;
}

void test8()
{
    X8 x;
    x.flag = 0 != 0;
}


/************************************************/

void foo9(float x)
{
    assert(x == 0.0f);
}

void len9(float x, float y, float z, float t)
{
    foo9(x*x+y*y+z*z);
}

void test9()
{
    float[4] a;
    a[0] = a[1] = a[2] = a[3] = 0.0f;

    for (int y = 0; y < 7; ++y)
    {
        len9(a[0], a[1], a[2], a[3]);

        float justOne() { return 1.0f; }

        float dot = justOne();
        if (dot < 0.0f)
            dot = 0.0f;
    }
}

/************************************************/

ubyte[4] arr10;

void foo10()
{
  *cast(float*)(&arr10[0]) = 3.25;
}

uint bar10()
{
  uint result = *cast(uint*)&arr10[0];
  return result;
}

float baz10()
{
  uint result = bar10();
  return *cast(float*)&result;
}

void test10()
{
  foo10();
  float x = baz10();

  assert(x == 3.25);
}


/************************************************/

interface I11
{
  void M ();
}

interface J11 : I11
{
  void N ();
}

class A11 : I11
{
  void M () { printf("A.M()\n"); }
}

class B11 : A11, J11
{
  void N () { printf("B.N()\n"); }
}

void test11()
{
  I11 f = new B11 ();

  f.M();
}


/************************************************/

int x12;

void test12()
{
    static class S
    {
        static this()
        {
            printf ("static constructor\n");
            x12 += 1;
        }

        this()
        {
            printf ("class constructor\n");
            x12 += 10;
        }
    }

    assert(x12 == 1);
    new S;
    assert(x12 == 11);
}


/************************************************/

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

    printf("Success\n");
    return 0;
}
