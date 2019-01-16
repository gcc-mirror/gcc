// PERMUTE_ARGS:
module m;

struct S {}
enum E { a }
void f() {}

void main()
{
     string ssc = S.stringof;   assert(ssc == "S"c);
    wstring ssw = S.stringof;   assert(ssw == "S"w);
    dstring ssd = S.stringof;   assert(ssd == "S"d);

     string esc = E.stringof;   assert(esc == "E"c);
    wstring esw = E.stringof;   assert(esw == "E"w);
    dstring esd = E.stringof;   assert(esd == "E"d);

     string msc = m.stringof;   assert(msc == "module m"c);
    wstring msw = m.stringof;   assert(msw == "module m"w);
    dstring msd = m.stringof;   assert(msd == "module m"d);

     string smc = S.mangleof;   assert(smc == "S1m1S"c);
    wstring smw = S.mangleof;   assert(smw == "S1m1S"w);
    dstring smd = S.mangleof;   assert(smd == "S1m1S"d);

     string fmc = f.mangleof;   assert(fmc == "_D1m1fFZv"c);
    wstring fmw = f.mangleof;   assert(fmw == "_D1m1fFZv"w);
    dstring fmd = f.mangleof;   assert(fmd == "_D1m1fFZv"d);

    // The default type is still string
    static assert(is(typeof(S.stringof) == string));
    static assert(is(typeof(E.stringof) == string));
    static assert(is(typeof(m.stringof) == string));
    static assert(is(typeof(S.mangleof) == string));
    static assert(is(typeof(f.mangleof) == string));
}
