// https://issues.dlang.org/show_bug.cgi?id=21438

int genGBPLookup() {
    static struct Table {
        int[1] entries;
    }

    auto table = new Table;
    auto x = table.entries[0];

    static assert(is(typeof(x) == int));
    return 0;
}

enum x = genGBPLookup;
