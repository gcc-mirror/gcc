// Pretend to be the GDC attributes module.
module gcc.attributes;

immutable value_ignored = 42;

enum type_symbol { _ }

auto template_symbol(A...)(A arguments)
{
    return arguments;
}

struct struct_wrong_field
{
    int bad_field;
}

struct struct_void_init
{
    string name_field = "noinline";
    int void_field = void;
    string ignored;
}

struct unknown_attribute
{
    string name_field = "made up name";
}
