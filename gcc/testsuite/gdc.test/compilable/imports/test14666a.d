module imports.test14666a;

auto getNames()
{
    import imports.test14666b;
    return "";
}

enum Names = getNames;
