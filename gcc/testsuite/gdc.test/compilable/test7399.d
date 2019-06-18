// https://issues.dlang.org/show_bug.cgi?id=7399
static assert(!__traits(compiles, { import non.existing.file; }));

// https://issues.dlang.org/show_bug.cgi?id=7400
static assert(!is(typeof({import non_existing_file;})));

