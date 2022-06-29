// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=106139
// { dg-do compile }

alias V = int[4];
alias A = float[4];

static if (__traits(compiles, __vector(V))):

A vector2array(__vector(V) v)
{
    return cast(A)v;
}

void vector2array(ref A a, __vector(V) v)
{
    a = cast(A)v;
}

__vector(V) array2vector(A a)
{
    return cast(__vector(V)) a;
}

void array2vector(ref __vector(V) v, A a)
{
    v = cast(__vector(V))a;
}
