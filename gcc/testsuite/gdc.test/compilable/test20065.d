alias AliasSeq(T...) = T;

void main()
{
    enum string[] array1 = [AliasSeq!("foo")];

    static assert(array1 == ["foo"]);

    enum string[] array2 = [AliasSeq!()];

    static assert(array2 == []);
}
