module protection.basic.tests;

import protection.basic.mod1;

static assert ( is(typeof(publicFoo())));
static assert ( is(typeof(packageFoo())));
static assert (!is(typeof(privateFoo())));

static assert ( is(typeof(Test.init.publicFoo())));
static assert (!is(typeof(Test.init.protectedFoo())));
static assert ( is(typeof(Test.init.packageFoo())));
static assert (!is(typeof(Test.init.privateFoo())));

class Deriv : Test
{
    void stub()
    {
        static assert ( is(typeof(this.publicFoo())));
        static assert ( is(typeof(this.protectedFoo())));
        static assert ( is(typeof(this.packageFoo())));
        static assert (!is(typeof(this.privateFoo())));
    }
}
