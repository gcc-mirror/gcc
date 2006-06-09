/* { dg-do compile } */

int decCompareOp (int result)
{
    if (result != (int)0x80000000)
    {
        result = -result;
        return (result > 0);
    }
    return 0;
}
