// https://issues.dlang.org/show_bug.cgi?id=21812

struct S(A...)
{
   A args;
}

static assert(__traits(allMembers, S!(int, float)) == AliasSeq!("args"));

alias AliasSeq(T...) = T;
