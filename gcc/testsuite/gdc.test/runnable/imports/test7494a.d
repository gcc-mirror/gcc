module imports.test7494a;

template map(alias fun)
{
    auto map(R)(R range) { return [4,5,6]; }
}

auto writeln(A...)(A args)
{
    return [7,8,9];
}

auto foo() {}
