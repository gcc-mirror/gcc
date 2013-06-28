/* PR tree-optimization/57441 */

int a, c, d, *e;
unsigned char b;

char
baz (char p1)
{
    return p1 * a;
}

void func_65 ();
func_1 ()
{
    func_65 ();
    func_65 ();
}

void
func_65 ()
{
    d = baz (b--);
    if (*e)
        b--;
    c = 0;
}
