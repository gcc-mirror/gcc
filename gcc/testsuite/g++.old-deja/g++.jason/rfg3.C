// Bug: g++ remembers the members of OUTER and complains about the second
// definition.
// Build don't link:

void
test ()
{
    {
	struct OUTER { struct INNER { int mbr; } member; };
    }

    {
	struct OUTER { struct INNER { int mbr; } member; };
    }
}
