// REQUIRED_ARGS: -O

void main()
{
    int[32] data;
    auto p1 = data.ptr + 0;
    auto p2 = data.ptr + 3;
    assert(p2 - p1 == 3);
}
