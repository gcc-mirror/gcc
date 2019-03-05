void dorecursive()
{
    recursive([0]);
}

void recursive(R)(R r)
{
    import std.algorithm;
    recursive( r.filter!(e=>true) );
}

