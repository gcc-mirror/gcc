// Build don't link: 
// GROUPS passed old-abort
class X
{
public:
    X ();
    int	f[4];
};

// Note that we mistakenly initialize the array data member as if it
// was scalar
X::X () : f (0) {}// ERROR - .*
