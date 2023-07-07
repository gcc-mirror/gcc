// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=98277
// { dg-do compile }

enum Side
{
    left,
    right
}

ref int getSide(Side side, return ref int left, return ref int right)
{
    return side == Side.left ? left : right;
}

enum SideA : int[]
{
    left = [0],
    right = [1],
}

int getSideA(SideA side, ref int left, ref int right)
{
    return side == SideA.left ? left : right;
}
