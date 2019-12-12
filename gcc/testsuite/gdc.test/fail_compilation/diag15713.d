/*
---
fail_compilation/diag15713.d(18): Error: no property 'widthSign' for type 'Data'
fail_compilation/diag15713.d(38): Error: template instance test.conwritefImpl!("parse-int", "width", "\x0a", Data()) error instantiating
fail_compilation/diag15713.d(43):        instantiated from here: conwritefImpl!("main", "\x0a", Data())
fail_compilation/diag15713.d(48):        instantiated from here: fdwritef!()
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
