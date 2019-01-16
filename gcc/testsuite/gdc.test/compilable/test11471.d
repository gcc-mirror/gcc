// REQUIRED_ARGS: -profile

void main() nothrow
{
    // Error: asm statements are assumed to throw
    version(GNU)
        asm { ""; }
    else
        asm { nop; }
}
