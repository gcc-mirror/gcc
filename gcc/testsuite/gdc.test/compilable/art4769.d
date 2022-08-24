// https://www.digitalmars.com/d/archives/digitalmars/D/bugs/4769.html

// COMPILED_IMPORTS: imports/art4769a.d imports/art4769b.d
// PERMUTE_ARGS:

module art4769;

private import imports.art4769a;

struct Vector(T)
{
    DataStreamability!(T).footype f;

    static if (DataStreamability!(T).isStreamable)
	void writeTo()
	{
	}
}
