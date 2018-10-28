module imports.test9672a;   // interpret

import test9672;            // node

class Type
{
    mixin ForwardCtor!();
}

//BasicType only created for standard types associated with tokens
class BasicType : Type
{
    static Type createType()
    {
        return null;
    }
}

class ValueT(T)
{
    Type getType()
    {
        return BasicType.createType();
    }
}
class CharValue : ValueT!char
{
    string toStr()
    {
        return null;
    }
}
