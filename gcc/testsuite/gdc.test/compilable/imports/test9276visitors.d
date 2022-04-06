module imports.test9276visitors;

template Visitors()
{
    mixin Semantic!(typeof(this));
    mixin DeepDup!(typeof(this));
}

import imports.test9276type;

template DeepDup(T) if (is(T : BasicType))
{}

template DeepDup(T)
{}
