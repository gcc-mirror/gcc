module imports.ice15138a;

alias AliasSeq(TL...) = TL;

alias FieldNameTuple(T...) = AliasSeq!();

struct TaggedAlgebraic(U)
{
    alias X = FieldNameTuple!(U.tupleof);
}

void get(T, U)(TaggedAlgebraic!U ta) {}

union PayloadUnion
{
    int dummy;
}

struct JSONValue
{
    alias Payload = TaggedAlgebraic!PayloadUnion;

    void get(T)()
    {
        Payload payload;
        .get!T(payload);
    }
}
