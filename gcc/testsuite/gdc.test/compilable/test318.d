// LINK:
// PERMUTE_ARGS: -version=C_Main

version (C_Main)
{
    // Fine, infers int
    extern(C) auto main(int argc, const char** argv)
    {
        return argc;
    }
}
else
{
    // Fine, infers void
    auto main()
    {

    }
}
