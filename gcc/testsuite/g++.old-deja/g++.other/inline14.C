// Build don't link:
// Origin: Gerald Pfeifer <pfeifer@dbai.tuwien.ac.at>

#include <iostream>

struct IDENT
    {
    enum TYPE { Variable, Constant } type;

    ostream& printTo(ostream& out) const
	{
	switch (type)
	    {
	    case Variable:
		out << '_';
		break;
	    default:
		break;
	    }
	return out;
	}
    };


template <class T>
struct TC
    {
    IDENT i;

    const IDENT& getIdent() const
        {
	return i;
	}
    };

template <class T>
inline ostream& operator<< (ostream& out, const TC<T> &c)
    {
    c.getIdent().printTo(out);
    return out;
    }

void foo(const TC<IDENT> &c)
    {
    cerr << c 
         << ": " // This line is crucial!
         << c
         << endl;
    }
