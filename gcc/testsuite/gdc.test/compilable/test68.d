// PERMUTE_ARGS:

// Bugzilla 4278

import imports.test68a;

class Foo : OtherModuleClass
{
        override void foo()
        {
                super.foo();
        }
}

void main()
{
        new Foo();
}

