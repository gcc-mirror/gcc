// { dg-do assemble  }
// GROUPS passed nested-classes
// The bug here is that wer'e getting a message about inner not
// being a basetype itself.  I think it's because it's being
// compared as the "inner" we knew about when it was forward-declared,
// versus the "inner" we know about when it *has* been defined.

class temp
{
public:
        struct inner;
        inner *trump()
        {
                return (tt);
        }
        struct inner
        {
                int ll;
        }*tt;
};
