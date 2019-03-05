// EXTRA_SOURCES: imports/test11745b.d
// REQUIRED_ARGS: -unittest
// PERMUTE_ARGS:
import imports.test11745b;

void main()
{
        // Test that we can invoke all unittests, including private ones.
        assert(__traits(getUnitTests, imports.test11745b).length == 3);
        foreach(test; __traits(getUnitTests, imports.test11745b))
        {
                test();
        }
}
