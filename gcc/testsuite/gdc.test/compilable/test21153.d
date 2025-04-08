// https://github.com/dlang/dmd/issues/21153
alias AliasSeq(TList...) = TList;
class DataClass;
void reduce(DataClass[] r)
{
    alias Args = AliasSeq!(DataClass);
    Args result = r[0];
}
