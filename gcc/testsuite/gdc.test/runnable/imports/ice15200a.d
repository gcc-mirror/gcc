module imports.ice15200a;
import imports.ice15200b;

auto f() // not void
{
    sub([0], false);
}
void sub(R)(R list, bool b)
{
    foreach (i; list.filter!(delegate(e) => b))
    {
    }
}
