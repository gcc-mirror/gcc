/*
TEST_OUTPUT:
---
fail_compilation/ice11404.d(10): Error: cannot have associative array of `(int, int)`
---
*/
template TypeTuple(TL...) { alias TL TypeTuple; }
void main()
{
    TypeTuple!(int, int)[string] my_map;
}
