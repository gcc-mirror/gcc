/* PR 17186 */

double foo()
{
    int i;
    double d;

    if (i)
        bar();
    else
        if (d) return 0;
}
