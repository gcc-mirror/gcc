// https://bugzilla.gdcproject.org/show_bug.cgi?id=66
// { dg-do run }
// { dg-skip-if "needs gcc/config.d" { ! d_runtime } }

void main()
{
    int pos = 0;

    foreach(x; 0 .. 64)
    {
        ++pos %= 4;
        assert (pos != 4);
    }
}
