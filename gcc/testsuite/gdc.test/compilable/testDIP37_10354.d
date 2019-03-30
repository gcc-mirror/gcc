// PERMUTE_ARGS:
// REQUIRED_ARGS: -o- -Icompilable/extra-files
// EXTRA_FILES: extra-files/pkgDIP37_10354/mbar.d
// EXTRA_FILES: extra-files/pkgDIP37_10354/mfoo.d
// EXTRA_FILES: extra-files/pkgDIP37_10354/package.d

module testDIP37_10354;
import pkgDIP37_10354.mfoo;
void main()
{
    import pkgDIP37_10354;
    foo!string();   // OK
    bar!string();   // OK <- ICE
}
