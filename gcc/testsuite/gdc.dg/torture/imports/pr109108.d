module imports.pr109108;
private enum int function(ref int)[] funs =
[
    0: (ref idx) => 0,
    1: (ref idx) => 1,
];

int test109108(I)(I idx)
{
    return funs[idx](idx);
}
