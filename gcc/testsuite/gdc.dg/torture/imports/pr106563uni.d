module imports.pr106563uni;

struct MultiArray()
{
    @property length()
    {
        return spaceFor!0();
    }
}

size_t spaceFor(size_t bits)()
{
    import imports.pr106563math;
    return nextPow2(bits);
}
