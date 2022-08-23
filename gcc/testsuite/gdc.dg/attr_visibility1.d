// { dg-do compile }
// { dg-require-visibility "" }

import gcc.attributes;

void nested()
{
    @attribute("visibility", "default")
    struct nested_struct { } // { dg-warning ".visibility. attribute ignored" }

    @attribute("visibility", "default")
    void nested_func() { } // { dg-warning ".visibility. attribute ignored" }
}

@attribute("visibility", 123)
int not_a_string(); // { dg-error ".visibility. attribute argument not a string constant" }

@attribute("visibility", "invalid argument")
int invalid_argument(); // { dg-error ".visibility. argument must be one of" }

@attribute("visibility", "default")
int redeclared_visibility();

@attribute("visibility", "internal")
int redeclared_visibility(); // { dg-error "redeclared with different visibility" }
