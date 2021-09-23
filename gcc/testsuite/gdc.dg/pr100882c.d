// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100882
// { dg-do compile }

struct CowArray
{
    this(this)
    {
    }
}

struct Tuple
{
    CowArray expand;
}

auto tuple(CowArray)
{
    return Tuple();
}

auto parseCharTerm()
{
    CowArray set;
    return tuple(set);
}
