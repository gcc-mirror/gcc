// https://issues.dlang.org/show_bug.cgi?id=18020

void bug(T)(T t)
{
    t.opCmp(t);
}

alias bugi = bug!(typeof(new class{}));
