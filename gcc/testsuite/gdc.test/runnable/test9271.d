// PERMUTE_ARGS:
// EXTRA_FILES: imports/test9271a.d
import algorithm = imports.test9271a;

bool any(alias predicate, Range)(Range range)
{
    return algorithm.any!(predicate)(range);
}

void main()
{
    auto arr = ["foo"];
    any!(e => e == "asd")(arr); // infinite recursive call -> OK
}
