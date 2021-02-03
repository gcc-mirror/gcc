// PERMUTE_ARGS: -version=A
// EXTRA_FILES: imports/link10920a.d

// It's imported but won't be linked.
import imports.link10920a;

void main()
{
    BitArray ba;
    version(A)
    {
        // Run semantic3 of BitArray.toString()
        // before the FormatSpec instantiation in main().
        static assert(is(typeof(ba.toString())));
    }

    // The instance codegen should be run always, unrelated with -version=A.
    FormatSpec!char fs;
    fs.func();
}
