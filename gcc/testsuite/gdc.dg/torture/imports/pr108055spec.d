module imports.pr108055spec;

template Unqual(T : const U, U)
{
    alias Unqual = U;
}

template FormatSpec(Char)
if (!is(Unqual!Char == Char))
{
    alias FormatSpec = FormatSpec!(Unqual!Char);
}

struct FormatSpec(Char)
if (is(Unqual!Char == Char))
{
    const(Char)[] nested;
}
