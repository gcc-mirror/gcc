module imports.template_ovs3;

/***************************************************/
// 1900 - template overload set

import imports.template_ovs1;
import imports.template_ovs2;

struct S1900
{
    alias .foo1900a foo1900a;
    alias .foo1900b foo1900b;

    alias .bar1900a bar1900a;
    alias .bar1900b bar1900b;

    alias .baz1900 baz1900;

    alias .bad1900 bad1900;

    // This is a kind of Issue 1528, cannot make overload contains both templates and functions
    //void funcF() {}
    //void funcT(T)(T) {}
    //alias funcF funca;   // make overload with alias declaration
    //alias funcT funca;
    //alias funcT funcb;   // make overload with alias declaration
    //alias funcF funcb;

    mixin Mix1900_A a;
    mixin Mix1900_B b;
}

/***************************************************/
// 1900

struct Traits1900(T) if (!is(T == class)) { enum name = "any"; }
