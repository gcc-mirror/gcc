template generateEmptyFunction(C, func...)
{
}

template isAbstractFunction(T...)
if (T.length == 1)
{
    enum bool isAbstractFunction = __traits(isAbstractFunction, T[0]);
}

alias BlackHole(Base) = AutoImplement!(Base, generateEmptyFunction, isAbstractFunction);

class AutoImplement(Base, alias how, alias what = isAbstractFunction) : Base {}
