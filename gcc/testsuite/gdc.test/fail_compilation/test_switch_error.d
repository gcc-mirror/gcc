/++
https://issues.dlang.org/show_bug.cgi?id=22514
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(13): Error: undefined identifier `doesNotExist`
fail_compilation/test_switch_error.d(16): Error: undefined identifier `alsoDoesNotExits`
fail_compilation/test_switch_error.d(19): Error: duplicate `case 2` in `switch` statement
---
++/

void test1()
{
    switch (doesNotExist)
    {
        case 1:
            alsoDoesNotExits();
            break;
        case 2: break;
        case 2: break;
    }
}

/++
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(105): Error: undefined identifier `doesNotExist`
---
++/
#line 100

enum foo = 1;

void test2()
{
    switch (doesNotExist)
    {
        case foo: break;
    }
}

/++
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(206): Error: undefined identifier `a`
fail_compilation/test_switch_error.d(207): Error: undefined identifier `b`
---
++/
#line 200

void test3()
{

    switch (1)
    {
        case a: break;
        case b: break;
    }
}

/++
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(303): Error: undefined identifier `doesNotExits`
---
++/
#line 300

void test4()
{
    auto foo = doesNotExits();
    switch (1)
    {
        case foo: break;
        case foo: break;
    }
}

/++
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(405): Error: `case` variables have to be `const` or `immutable`
fail_compilation/test_switch_error.d(412): Error: `case` variables not allowed in `final switch` statements
---
++/
#line 400

void test5(int i)
{
    switch (i)
    {
        case i: break;
        default: break;
    }

    const int j = i;
    final switch (i)
    {
        case j: break;

    }
}

/++
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(513): Error: undefined identifier `undefinedFunc`
fail_compilation/test_switch_error.d(517): Error: `case` expression must be a compile-time `string` or an integral constant, not `Strukt(1)`
fail_compilation/test_switch_error.d(518): Error: `case` variables have to be `const` or `immutable`
fail_compilation/test_switch_error.d(518): Error: `case` variables not allowed in `final switch` statements
fail_compilation/test_switch_error.d(519): Error: `case` variables not allowed in `final switch` statements
fail_compilation/test_switch_error.d(522): Error: undefined identifier `undefinedFunc2`
---
++/
#line 500

enum Foo
{
   one, two
}

struct Strukt
{
    int i;
}

void errorsWithErrors(int param, immutable int constant)
{
   final switch(undefinedFunc())
   {
      case Foo.one:     break;
      case Foo.two:     break;
      case Strukt(1):   break;
      case param:       break;
      case constant:    break;
   }

   switch (undefinedFunc2())
   {
       case constant:   break;
   }
}

/++
TEST_OUTPUT:
---
fail_compilation/test_switch_error.d(622): Error: undefined identifier `undefinedFunc`
fail_compilation/test_switch_error.d(624): Error: `case` expression must be a compile-time `string` or an integral constant, not `SubtypeOfInt(2)`
fail_compilation/test_switch_error.d(625): Error: `case` expression must be a compile-time `string` or an integral constant, not `SubtypeOfIntMethod()`
---
++/
#line 600

struct SubtypeOfInt
{
    int i;
    alias i this;
}

struct SubtypeOfIntMethod
{
    int getI() { return 0; }
    alias getI this;
}

void errorsWithErrors2(int param)
{
    final switch(param)
    {
        case SubtypeOfInt(1):         break;
        case SubtypeOfIntMethod():    break;
    }

    // This snippet causes somewhat misleading error messages
    final switch(undefinedFunc())
    {
        case SubtypeOfInt(2):         break;
        case SubtypeOfIntMethod():    break;
    }
}
