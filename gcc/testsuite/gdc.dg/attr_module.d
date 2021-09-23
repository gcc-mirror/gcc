// { dg-do compile }
// { dg-additional-sources "imports/attributes.d" }

import gcc.attributes;

@value_ignored
int f0()
{
    return 0;
}

@type_symbol // { dg-warning ".type_symbol. attribute has no effect" }
int f1()
{
    return 1;
}

@template_symbol // { dg-warning ".template_symbol. attribute has no effect" }
int f2()
{
    return 2;
}

@struct_wrong_field(123) // { dg-warning  "unknown attribute .123." }
int f3()
{
    return 3;
}

@struct_void_init()
int f4()
{
    return 4;
}

@unknown_attribute() // { dg-warning "unknown attribute .made up name." }
int f5()
{
    return 5;
}
