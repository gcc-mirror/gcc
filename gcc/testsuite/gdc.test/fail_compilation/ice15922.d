/*
TEST_OUTPUT:
---
fail_compilation/ice15922.d(23): Error: function `ice15922.ValidSparseDataStore!int.ValidSparseDataStore.correctedInsert!false.correctedInsert` has no `return` statement, but is expected to return a value of type `int`
fail_compilation/ice15922.d(21): Error: template instance `ice15922.ValidSparseDataStore!int.ValidSparseDataStore.correctedInsert!false` error instantiating
fail_compilation/ice15922.d(26):        instantiated from here: `ValidSparseDataStore!int`
fail_compilation/ice15922.d(14): Error: calling non-static function `insert` requires an instance of type `ValidSparseDataStore`
fail_compilation/ice15922.d(26): Error: template instance `ice15922.StorageAttributes!(ValidSparseDataStore!int)` error instantiating
---
*/

template StorageAttributes(Store)
{
    enum hasInsertMethod = Store.insert;
    enum hasFullSlice = Store.init[];
}
struct ValidSparseDataStore(DataT)
{
    DataT insert()
    {
        correctedInsert!(false);
    }
    DataT correctedInsert(bool CorrectParents)() {}
    auto opSlice() inout {}
}
alias BasicCubeT = StorageAttributes!(ValidSparseDataStore!int);
