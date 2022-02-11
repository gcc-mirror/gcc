// https://issues.dlang.org/show_bug.cgi?id=20296

extern (C++)
void foo(T...)(T args)
{
}

alias F = foo!(char*);
