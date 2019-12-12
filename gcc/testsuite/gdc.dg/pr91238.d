// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=91238
// { dg-do compile }

alias T = const(char)*;

T name()
{
    return "";
}

void collect(ref T)
{
}

void configure(T[T] targets)
{
    collect(targets[name]);
}
