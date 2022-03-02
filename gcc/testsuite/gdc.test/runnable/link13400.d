// COMPILE_SEPARATELY:
// EXTRA_SOURCES: imports/link13400a.d

import imports.link13400a;

void main()
{
    BigInt r;

    // This comparison will instantiate BigInt.opEquals!().opEquals(const BigInt y) const pure again.
    // But here is non-speculative context, so this module compilation should generate its objcode.
    bool b = r == BigInt("2");  // comparison with rvalue
}
