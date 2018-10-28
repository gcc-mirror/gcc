module imports.link13400a;

struct BigInt
{
    this(string s) {}

    // This template opEquals is determined to 'identity opEquals' by
    // hasIdentityOpEquals() in clone.c with the speculative instantiation
    // with dummy rvalue argument.
    // Then BigInt.opEquals!().opEquals(const BigInt y) const pure is stored
    // in template instance cache.
    bool opEquals()(auto ref const BigInt y) const pure
    {
       return true;
    }
}
