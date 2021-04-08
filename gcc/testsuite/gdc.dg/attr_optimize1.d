// { dg-do compile }

import gcc.attributes;

int func()
{
    int return_zero() @optimize(0)
    {
        return 0;
    }

    int return_one() @optimize("0")
    {
        return 1;
    }

    int return_two() @optimize("s")
    {
        return 2;
    }

    int return_three() @optimize("O3")
    {
        return 3;
    }

    int return_four() @optimize("fast-math")
    {
        return 4;
    }

    return return_one + return_two + return_three + return_four;
}

@optimize(3)
int var = 0; // { dg-warning ".optimize. attribute ignored" }

@optimize("-f_")
int bad_option() // { dg-warning "bad option .-f_. to attribute .optimize." }
{
    return 0;
}

@optimize("-z")
int bad_option2() // { dg-warning "bad option .-z. to attribute .optimize." }
{
    return 0;
}
