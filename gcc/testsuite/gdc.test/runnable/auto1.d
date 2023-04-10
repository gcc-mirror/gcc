/*
RUN_OUTPUT:
---
Foo.~this()
Foo.~this()
Foo.~this()
Foo.~this()
A2.this()
Hello world.
A2.~this()
Success
---
*/

import core.stdc.stdio;

/******************************************/

class Foo
{
    static int x;

    ~this()
    {
        printf("Foo.~this()\n");
        x++;
    }
}

int test1x()
{
    scope Foo f = new Foo();
    return 6;
}


void test1()
{
    {
        scope Foo f = new Foo();
    }
    int c;

    assert(Foo.x == 1);
    c = test1x();
    assert(c == 6);
    assert(Foo.x == 2);

    if (c != 6)
        scope Foo h = new Foo();
    assert(Foo.x == 2);

    if (c == 6)
        scope Foo j = new Foo();
    assert(Foo.x == 3);

    {
        scope Foo g = null, k = new Foo();
        assert(Foo.x == 3);
    }
    assert(Foo.x == 4);
}

/******************************************/

int ax;

class A2
{
  this() scope
  {
    printf("A2.this()\n");
    ax += 1;
  }

  ~this() scope
  {
    printf("A2.~this()\n");
    ax += 1000;
  }
};


void test2()
{
  {
    scope A2 a = new A2();
    printf("Hello world.\n");
  }
  assert(ax == 1001);
}



/******************************************/

int status3;

class Parent3
{
}

class Child3 : Parent3
{
        this() scope {
                assert(status3==0);
                status3=1;
        }

        ~this() scope {
                assert(status3==1);
                status3=2;
        }
}

void foo3()
{
        scope Parent3 o = new Child3();
        assert(status3==1);
}

void test3()
{
        foo3();
        assert(status3==2);
}

/******************************************/

int main()
{
    test1();
    test2();
    test3();

    printf("Success\n");
    return 0;
}
