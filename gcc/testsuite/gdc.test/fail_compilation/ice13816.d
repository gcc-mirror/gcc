/*
TEST_OUTPUT:
---
fail_compilation/ice13816.d(17): Error: template instance `TypeTuple!(ItemProperty!())` recursive template expansion
fail_compilation/ice13816.d(17): Error: alias `ice13816.ItemProperty!().ItemProperty` recursive alias declaration
fail_compilation/ice13816.d(22): Error: template instance `ice13816.ItemProperty!()` error instantiating
---
*/


alias TypeTuple(T...) = T;

template ItemProperty()
{
    static if (true)
    {
        alias ItemProperty = TypeTuple!(ItemProperty!());
    }
}
void main()
{
    alias items = ItemProperty!();

    enum num = items.length;
}
