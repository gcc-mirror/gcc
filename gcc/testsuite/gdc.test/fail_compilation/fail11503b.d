immutable int[] x = [1, 2, 3];

auto makes() pure
{
    return x;
}

int main()
{
    auto a = x;
    int[] b = makes();
    return b[1];
}
