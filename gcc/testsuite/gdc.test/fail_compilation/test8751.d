Bar foo3(ref const int x) pure {
    return y => x > y; // error
}
