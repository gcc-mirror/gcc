// https://bugzilla.gdcproject.org/show_bug.cgi?id=280
// { dg-do compile }

struct RBNode280
{
    RBNode280* _parent;

    @property left(RBNode280*)
    {
        _parent = &this;
    }
}

class RedBlackTree280
{
    RBNode280* _end;
    RBNode280* _begin;

    this(int[] elems...)
    {
        _end = new RBNode280;

        foreach (e; elems)
        {
            _end.left = _begin;
        }
    }
}

__gshared s = new RedBlackTree280('h');
