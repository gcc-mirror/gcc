// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

bool normalize(double[] range, double sum = 1)
{
    double s = 0;
    const length = range.length;
    foreach (e; range)
    {
        s += e;
    }
    if (s == 0)
    {
        return false;
    }
    return true;
}

void main()
{
    double[3] range = [0.0, 0.0, 0.0];
    assert(normalize(range[]) == false);
    range[1] = 3.0;
    assert(normalize(range[]) == true);
}
