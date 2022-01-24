// https://issues.dlang.org/show_bug.cgi?id=21479
enum Side
{
    left,
    right
}

struct Both(T)
{
    T left;
    T right;

    ref T get(Side side)
    {
        return side == Side.left ? left : right;
    }
}

void main()
{
    Both!(int[]) t;
    t.get(Side.left) ~= 1;
    assert (t.left.length == 1);

    t.get(Side.right) ~= 1;
    t.get(Side.right) ~= 2;
    assert (t.right.length == 2);
}
