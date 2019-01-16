// PERMUTE_ARGS: -inline -release -g -O

auto serialize(T)(T value)
{
    foreach (i; value) { }

    return;     // important
    // By this ReturnStatement with NULL exp wrongly appears in the
    // expanded result of serialize(["test"]) call in main(), it will
    // return from main() without setting exit code.
}

int main()
{
    serialize(["test"]);
    return 0;
}
