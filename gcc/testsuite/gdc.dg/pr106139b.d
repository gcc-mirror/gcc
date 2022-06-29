// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=106139
// { dg-do compile }

alias A = float[4];

static if (__traits(compiles, __vector(A))):

A vector2array(__vector(A) v)
{
    return cast(A)v;
}

void vector2array(ref A a, __vector(A) v)
{
    a = cast(A)v;
}

__vector(A) array2vector(A a)
{
    return cast(__vector(A)) a;
}

void array2vector(ref __vector(A) v, A a)
{
    v = cast(__vector(A))a;
}

A vector2array_array(__vector(A) v)
{
    return v.array;
}

void vector2array_array(ref A a, __vector(A) v)
{
    a = v.array;
}
