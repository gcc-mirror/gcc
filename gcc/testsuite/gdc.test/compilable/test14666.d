// REQUIRED_ARGS: -o-
// EXTRA_FILES: imports/test14666a.d imports/test14666b.d
// PERMUTE_ARGS:
module test14666;

struct Location
{
    import imports.test14666a;
}
