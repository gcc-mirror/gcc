// https://issues.dlang.org/show_bug.cgi?id=23863

alias AliasSeq(T...) = T;

struct S
{
}
alias Empty = S.tupleof;
Empty x; // accepts valid

static assert(is(typeof(x)));
static assert(is(typeof(Empty)));
static assert(is(typeof(AliasSeq!(int))));
static assert(is(typeof(Empty) == AliasSeq!()));
static assert(is(typeof(AliasSeq!()) == AliasSeq!()));
