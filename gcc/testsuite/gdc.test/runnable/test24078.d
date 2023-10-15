//https://issues.dlang.org/show_bug.cgi?id=24078

void main()
{
    assert(["c"] ~ "a" ~ "b" == ["c", "a", "b"]);
}
