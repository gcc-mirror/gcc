module imports.test67a;

import test67;

class Base
{
    I create() {
        return null;
    }
}

class Derived : Base
{
    override SubI create() {
        return null;
    }
}
