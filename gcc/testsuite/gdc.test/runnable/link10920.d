// PERMUTE_ARGS: -version=A

// It's imported but won't be linked.
import imports.link10920a;

void main()
{
    BitArray ba;
    version(A)
    {
        // Run semantic3 of BitArray.toString()
        // before the FormatSpec instantiation in main().
        pragma(msg, typeof(ba.toString()));
    }

    // The instance codegen should be run always, unrelated with -version=A.
    FormatSpec!char fs;
    fs.func();
}
