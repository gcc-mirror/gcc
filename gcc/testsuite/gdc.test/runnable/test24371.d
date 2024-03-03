// https://issues.dlang.org/show_bug.cgi?id=24371

void main()
{
    assert("b" ~ "c" == "bc");
    assert(["a"] ~ "b" == ["a", "b"]);
    assert(["a"] ~ ("b" ~ "c") == ["a", "bc"]);

    auto strArr = ["a"];
    assert(strArr ~ ("b" ~ "c") == ["a", "bc"]);
    auto str = "c";
    assert(["a"] ~ ("b" ~ str) == ["a", "bc"]);

    assert(strArr ~ ("b" ~ str) == ["a", "bc"]);
}
