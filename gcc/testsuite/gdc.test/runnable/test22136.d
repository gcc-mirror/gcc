
interface IObject
{
    size_t toHash() @trusted nothrow;
}

interface Dummy {}
interface Bug(E) : Dummy, IObject {}
interface OK(E) : IObject, Dummy {}

void main()
{

    {
        Bug!string s;
        size_t t = hashOf(s);
    }
    {
        OK!string s;
        size_t t = hashOf(s);
    }

    static assert(is(immutable Bug!string* : immutable IObject*));
    static assert(is(immutable OK!string* : immutable IObject*));
}
