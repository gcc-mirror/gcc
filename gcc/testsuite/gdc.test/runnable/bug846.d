// see also: bug 8
// EXTRA_SOURCES: imports/bug846.d

import imports.bug846;

void main()
{
    auto num = removeIf( "abcdef".dup, ( char c ) { return c == 'c'; } );
    assert(num == 5);
}

