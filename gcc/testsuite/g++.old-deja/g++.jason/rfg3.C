// { dg-do assemble  }
// Bug: g++ remembers the members of OUTER and complains about the second
// definition.

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
