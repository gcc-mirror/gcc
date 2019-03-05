module imports.gdc231;

interface ImplementorIF
{
    void* getImplementorStruct();
    void* getStruct();
}

template ImplementorT()
{
    void* getImplementorStruct()
    {
        return null;
    }
}

class Widget231 : ImplementorIF
{
    mixin ImplementorT;
    void* getStruct()
    {
        return null;
    }
}
