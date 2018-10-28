module imports.link11127a;

struct Cycle(Range)
{
    alias Range R;

    R _original;
    size_t _index;

    this(R input, size_t index = 0) {}
}

Cycle!R cycle(R)(R input)
{
    return Cycle!R(input);
}

Cycle!R cycle(R)(R input, size_t index = 0)
{
    return Cycle!R(input, index);
}
