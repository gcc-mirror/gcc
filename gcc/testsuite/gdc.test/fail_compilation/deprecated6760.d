// REQUIRED_ARGS: -de

/*
TEST_OUTPUT:
---
fail_compilation/deprecated6760.d(13): Deprecation: function deprecated6760.Foo.opEquals overridden functions cannot be annotated @disable
fail_compilation/deprecated6760.d(18): Deprecation: function deprecated6760.Bar.opEquals deprecated functions cannot be annotated @disable
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

