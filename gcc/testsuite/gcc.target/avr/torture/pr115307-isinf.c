/* { dg-do compile } */

int call_isinff (float f)
{
    int isinff (float);
    return isinff (f);
}

int call_isinf (double f)
{
    int isinf (double);
    return isinf (f);
}

int call_isinfl (long double f)
{
    int isinfl (long double);
    return isinfl (f);
}

/* { dg-final { scan-assembler-not "unord" } } */
