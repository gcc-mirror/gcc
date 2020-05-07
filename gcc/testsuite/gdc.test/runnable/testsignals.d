// RUNNABLE_PHOBOS_TEST
import std.stdio;
import std.signals;

class Observer
{   // our slot
    void watch(string msg, int i)
    {
        writefln("Observed msg '%s' and value %s", msg, i);
    }

    void watch2(int i, int j)
    {
        writefln("Observed msg %s,%s", i, j);
    }
}

class Foo
{
    int value() { return _value; }

    int value(int v)
    {
        if (v != _value)
        {   _value = v;
            // call all the connected slots with the two parameters
            emit("setting new value", v);
        }
        return v;
    }

    // Mix in all the code we need to make Foo into a signal
    mixin Signal!(string, int);

  private :
    int _value;
}

void test1()
{
    Foo a = new Foo;
    Observer o = new Observer;

    a.value = 3;                // should not call o.watch()
    a.connect(&o.watch);        // o.watch is the slot
    a.value = 4;                // should call o.watch()
    a.disconnect(&o.watch);     // o.watch is no longer a slot
    a.value = 5;                // so should not call o.watch()
    a.connect(&o.watch);        // connect again
    a.value = 6;                // should call o.watch()
    delete o;                   // destroying o should automatically disconnect it
    a.value = 7;                // should not call o.watch()
}

/******************************************/

class Input
{
    mixin Signal!(int, int) click;
    mixin Signal!(char) keyDown;
}

void test2()
{
    Observer o = new Observer();
    Input a = new Input();
    a.click.connect(&o.watch2);
    a.click.emit(5,6);
}

/******************************************/

class Args3
{
        int foo;
}

class Base3
{
        ~this()
        {
                writefln("Base3 dtor!");
        }
}

class Test3 : Base3
{
        mixin Signal!(Args3) A;
        mixin Signal!(Args3) B;

        ~this()
        {
                writefln("Test3 dtor");
        }
}


void test3()
{
        auto test = new Test3;
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
