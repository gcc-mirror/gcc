// https://issues.dlang.org/show_bug.cgi?id=23386
// Segfault on enum member UDA inside template

template E()
{
    enum E : byte
    {
        @(1) none,
    }
}

alias T = E!();
