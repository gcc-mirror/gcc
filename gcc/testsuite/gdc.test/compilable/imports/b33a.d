module imports.b33a;

struct IsEqual( T )
{
    bool opCall( char p1, char p2 )
    {
	return p1 == p2;
    }
}

template find_( Elem, Pred = IsEqual!(Elem) )
{
    size_t fn( char[] buf, Pred pred = Pred.init )
    {
        return 3;
    }
}

template find()
{
    size_t find( char[3] buf )
    {
        return find_!(char).fn( buf );
    }
}
