// { dg-do compile }

import gcc.attributes;

@optimize(-1)
int non_negative() // { dg-error "argument to .-O. should be a non-negative integer" }
{
    return 0;
}
