module imports.test35a;

template ElemTypeOf( T )
{
    alias typeof(T.init[0]) ElemTypeOf;
}

template removeIf_( Elem, Pred )
{
    size_t fn( Elem[] buf, Pred pred )
    {
	void exch( size_t p1, size_t p2 )
	{
	    Elem t  = buf[p1];
	    buf[p1] = buf[p2];
	    buf[p2] = t;
	}

	size_t cnt = 0;

	for( size_t pos = 0, len = buf.length; pos < len; ++pos )
	{
	    if( pred( buf[pos] ) )
		++cnt;
	    else
		exch( pos, pos - cnt );
	}
	return buf.length - cnt;
    }
}

template removeIf( Buf, Pred )
{
    size_t removeIf( Buf buf, Pred pred )
    {
	return removeIf_!(ElemTypeOf!(Buf), Pred).fn( buf, pred );
    }
}

