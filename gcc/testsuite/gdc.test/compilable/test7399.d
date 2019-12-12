// 7399
static assert(!__traits(compiles, { import non.existing.file; }));

// 7400
static assert(!is(typeof({import non_existing_file;})));

