// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=98277
// { dg-do compile }

enum Side
{
    left,
    right
}

ref int getSide(Side side, ref int left, ref int right)
{
    return side == Side.left ? left : right;
}
