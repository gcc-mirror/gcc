int foo (float x)
{
        float i = __builtin_inff ();
        return x != i;
}
