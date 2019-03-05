
// COMPILE_SEPARATELY
// EXTRA_SOURCES: imports/test11039b.d

import imports.test11039b;

struct SomeStruct(T)
{
    T field;
    T getInnerField()
    {
        return field;
    }
}

static globalField = SomeStruct!string("Hello!");

void main()
{
    globalField.getInnerField();
    anotherGlobalField.getInnerField();
}

