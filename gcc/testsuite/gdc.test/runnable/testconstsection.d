
string tls_var = "tls_string";

__gshared string data_var = "data_string";

__gshared string bss_var;

struct Range
{
    const(void)* bot;
    const(void)* top; // consider inclusive

    void addPtr(const(void)* p)
    {
        if (!bot || p < bot)
            bot = p;
        if (!top || p > top)
            top = p;
    }

    bool intersect(Range other)
    {
        return (bot <= other.top && top >= other.bot);
    }
}

void testStrings()
{
    // check that the strings don't overlap with the variables
    Range tls;
    Range data;
    Range bss;
    Range cdata;

    static string local_tls_var = "tls_string";
    static __gshared string local_data_var = "data_string";
    static __gshared string local_bss_var;

    tls.addPtr(&tls_var);
    tls.addPtr(&local_tls_var);

    data.addPtr(&data_var);
    data.addPtr(&local_data_var);

    bss.addPtr(&bss_var);
    bss.addPtr(&local_bss_var);

    cdata.addPtr(tls_var.ptr);
    cdata.addPtr(local_tls_var.ptr);
    cdata.addPtr(data_var.ptr);
    cdata.addPtr(local_data_var.ptr);

    assert(!cdata.intersect(tls),  "overlap with tls");
    assert(!cdata.intersect(data), "overlap with data");
    assert(!cdata.intersect(bss),  "overlap with bss");
}

void main()
{
    testStrings();
}
