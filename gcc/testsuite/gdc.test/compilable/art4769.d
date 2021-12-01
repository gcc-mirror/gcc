// http://www.digitalmars.com/webnews/newsgroups.php?art_group=digitalmars.D.bugs&article_id=4769

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

