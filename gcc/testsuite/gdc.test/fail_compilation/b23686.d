/*
TEST_OUTPUT:
---
fail_compilation/b23686.d(107): Error: undefined identifier `eFN1`, did you mean template `eFN0()()`?
fail_compilation/b23686.d(107): Error: `mixin(_error_)` does not give a valid type
fail_compilation/b23686.d(115):        while looking for match for `eload!(int, 1)`
fail_compilation/b23686.d(121): Error: undefined identifier `FNwtf`
fail_compilation/b23686.d(121): Error: `mixin(_error_)` does not give a valid type
fail_compilation/b23686.d(126):        while looking for match for `load!"wtf"`
---
*/
module b23686;

#line 100

//-------------------

void eFN0()()
{
}

void eload(I, I name,  alias T = mixin("eFN" ~ name.stringof))()
{
    T!()();
}

void test2()
{
    eload!(int,0)();
    eload!(int,1)();
}

//-------------------

void FNfoo() {}
void load(string name, alias T = mixin("FN" ~ name))() {}

void test1()
{
    load!"foo"();
    load!"wtf"();
}
