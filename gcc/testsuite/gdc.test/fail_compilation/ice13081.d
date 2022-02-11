/*
TEST_OUTPUT:
---
fail_compilation/ice13081.d(17): Error: undefined identifier `node`
fail_compilation/ice13081.d(17): Error: undefined identifier `data`
fail_compilation/ice13081.d(17): Error: undefined identifier `node`
fail_compilation/ice13081.d(28): Error: template instance `ice13081.Cube!(SparseDataStore)` error instantiating
---
*/

struct Cube(StorageT)
{
    StorageT datastore;
    alias datastore this;
    auto seed()
    {
        this[] = node.data ? data : node.data;
    }
}

class SparseDataStore
{
    auto opSlice() {}
}

void main()
{
    Cube!SparseDataStore c;
}
