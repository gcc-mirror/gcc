// REQUIRED_ARGS: -de

/*
TEST_OUTPUT:
---
fail_compilation/deprecated6760.d(13): Deprecation: `deprecated6760.Foo.opEquals` cannot be annotated with `@disable` because it is overriding a function in the base class
fail_compilation/deprecated6760.d(18): Deprecation: `deprecated6760.Bar.opEquals` cannot be marked as `deprecated` because it is overriding a function in the base class
---
*/

class Foo
{
    @disable override bool opEquals(Object);
}

class Bar
{
    deprecated override bool opEquals(Object);
}

