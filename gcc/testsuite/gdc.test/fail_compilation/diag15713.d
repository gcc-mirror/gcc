/*
TEST_OUTPUT:
---
fail_compilation/diag15713.d(19): Error: no property `widthSign` for `this` of type `diag15713.WrData.Data`
fail_compilation/diag15713.d(39): Error: template instance `diag15713.conwritefImpl!("parse-int", "width", "\n", Data(null))` error instantiating
fail_compilation/diag15713.d(44):        instantiated from here: `conwritefImpl!("main", "\n", Data(null))`
fail_compilation/diag15713.d(49):        instantiated from here: `fdwritef!()`
---
*/

void wrWriteWidthChar() {}

auto WrData(int , int )
{
    struct Data
    {
        auto initInt(string name)()
        {
            __traits(getMember, this, name ~ "Sign");
        }
    }
    return Data();
}

template conwritefImpl(string state, string field, string fmt, alias data, AA...)
if (state == "parse-int")
{
    enum conwritefImpl = data.initInt!field;
}

template baz(string state, string fmt, alias data, AA...) {}
template bar(string state, string fmt, alias data, AA...) {}

    enum a = "parse-format";

template conwritefImpl(string state, string fmt, alias data, AA...)
if (state == "main")
{
    enum conwritefImpl = conwritefImpl!("parse-int", "width", fmt, data);
}

void fdwritef()()
{
    conwritefImpl!("main", "\n", WrData(0, 0));
}

void conwriteln()
{
    fdwritef();
}
