// GROUPS passed templates nested-classes
// Special g++ Options: 
//
// The SetLS template test
//
// Wendell Baker, Berkeley CAD Group, 1993 (wbaker@ic.Berkeley.EDU)
//


#pragma implementation "ListS.h"
#pragma implementation "SetLS.h"

#include <stdlib.h>
#include <iostream.h>
# 1 "../../templates/SetLS.h" 1
// -*- C++ -*-



//
// A Set Template - implemented with an ListS
//
// Wendell Baker, Berkeley CAD Group, 1993 (wbaker@ic.Berkeley.EDU)
//





#pragma interface





#define XTRUE true
#define XFALSE false

# 37 "../../templates/SetLS.h"


# 1 "../../templates/ListS.h" 1
// -*- C++ -*-



//
// A List Template - providing a singly linked capability
//
// Wendell Baker, Berkeley CAD Group, 1993 (wbaker@ic.Berkeley.EDU)
//





#pragma interface






# 1 "/projects/gnu-cygnus/gnu-cygnus-14/mips/lib/gcc-lib/decstation/cygnus-reno-1/g++-include/bool.h" 1 3
// Defining XTRUE and XFALSE is usually a Bad Idea,
// because you will probably be inconsistent with anyone
// else who had the same clever idea.
// Therefore:  DON'T USE THIS FILE.









# 26 "../../templates/ListS.h" 2

# 37 "../../templates/ListS.h"



// g++ reno-1 is not yet capable of creating templates with nested
// classes which instantiate the template arguments.
template<class T>
struct ListS_link {
    T item;
    ListS_link<T> *next;

    ListS_link(const T& i, ListS_link<T> *n = 0): item(i), next(n)
	{ }
};


//
// For now, errors are raised by ::abort() because exceptions
// are not well implemented in cxx or at all in CC 3.0.1
//
template<class T>
class ListS {
public:
    ListS();
    ListS(const ListS<T>&);
    ~ListS();

    void operator=(const ListS<T>&);
    
    unsigned length() const
	{ return count; }
    
    void prepend(const T& item);
    void append(const T& item);
    void clear();

    const T& head() const
	{ ensure_1();
	  return head_link->item; }
    T& head()
	{ ensure_1();
	  return head_link->item; }
    void head(T& fill) const
	{ ensure_1();
	  fill = head_link->item; }
    void remove_head()
	{ remove_head_filling(0); }
    void remove_head(T& fill)
	{ remove_head_filling(&fill); }

    const T& tail() const
	{ ensure_1();
	  return tail_link->item; }
    T& tail()
	{ ensure_1();
	  return tail_link->item; }
    void tail(T& fill) const
	{ ensure_1();
	  fill = tail_link->item; }

    class Vix {
    public:
	Vix(): owner(0), index(0)
	    { }
	
	// These are friend functions so that v == x is the same as x == v
	friend int operator==(void *v, const Vix& x)
	    { return v == x.index; }
	friend int operator==(const Vix& x, void *v)
	    { return v == x.index; }
	friend int operator!=(void *v, const Vix& x)
	    { return v != x.index; }
	friend int operator!=(const Vix& x, void *v)
	    { return v != x.index; }
	friend int operator==(const Vix& x1, const Vix& x2)
	    { return x1.owner == x2.owner && x1.index == x2.index; }
	friend int operator!=(const Vix& x1, const Vix& x2)
	    { return x1.owner != x2.owner || x1.index != x2.index; }
    private:
        friend class ListS<T>;
	

	Vix(const ListS<T> *o, ListS_link<T> *i): owner(o), index(i)
	    { }




	
	const ListS<T> *owner;

	ListS_link<T> *index;



    };
    
    Vix first() const
	{ return Vix(this, head_link); }
    void next(Vix& x) const
	{ check(x);
	  if (x.index != 0)
	      x.index = x.index->next; }
    T& operator()(const Vix& x)
	{ check(x);
	  return x.index->item; }
    const T& operator()(const Vix& x) const
	{ check(x);
	  return x.index->item; }
protected:
# 154 "../../templates/ListS.h"


    unsigned count;

    ListS_link<T> *head_link;	// 0 for a zero-length list
    ListS_link<T> *tail_link;	// 0 for a zero-length list





private:
    // fill may be 0 (then don't fill)
    void remove_head_filling(T *fill);

    void ensure_1() const
	{ if (0 == head_link)
	      ::abort(); }
    void check(const Vix& x) const
	{ if (this != x.owner)
	      ::abort();
	  if (0 == x.index)
	      ::abort(); }
};

template<class T>
ListS<T>::ListS():
count(0),
head_link(0),
tail_link(0)
{ }

template<class T>
ListS<T>::ListS(const ListS<T>& other):
count(0),
head_link(0),
tail_link(0)
{
    for (Vix x=other.first(); 0 != x; other.next(x))
	append(other(x));
}

template<class T>
ListS<T>::~ListS()
{
    clear();
}

template<class T>
void
ListS<T>::operator=(const ListS<T>& other)
{
    clear();
    for (Vix x=other.first(); 0 != x; other.next(x))
	append(other(x));
}

template<class T>
void
ListS<T>::prepend(const T& item)
{

    head_link = new ListS_link<T>(item, head_link);



    if (0 == tail_link)
	tail_link = head_link;
    count++;
}

template<class T>
void
ListS<T>::append(const T& item)
{

    ListS_link<T> *new_link = new ListS_link<T>(item); 



    if (0 == tail_link) {
	head_link = new_link;
	tail_link = new_link;
    } else {
	tail_link->next = new_link;
	tail_link = tail_link->next;
    }
    count++;
}

template<class T>
void
ListS<T>::clear()
{

    ListS_link<T> *next, *l;



    for (l=head_link; 0 != l; l=next) {
	next = l->next;
	delete l;
    }

    count = 0;
    head_link = 0;
    tail_link = 0;
}

template<class T>
void
ListS<T>::remove_head_filling(T* fill)
// fill may be 0 in which case don't assign into it
{
    ensure_1();

    ListS_link<T> *ohead = head_link;



    if (0 != fill)
	*fill = ohead->item;
    head_link = ohead->next;
    if (0 == head_link)
	tail_link = 0;
    count--;
    delete ohead;
}


# 39 "../../templates/SetLS.h" 2


# 62 "../../templates/SetLS.h"

template<class T>
class SetLS {
public:
    SetLS();
    
    void add(const T& item);
    // There is no remove(const T& item) for this set
    bool contains(const T& item) const;

    unsigned length() const
	{ return list.length(); }

    void clear()
	{ list.clear(); }

    class Vix {
    public:
	Vix(): owner(0), vix()
	    { }

	// These are friend functions so that v == x is the same as x == v
	friend int operator==(void *v, const Vix& x)
	    { return v == x.vix; }
	friend int operator==(const Vix& x, void *v)
	    { return v == x.vix; }
	friend int operator!=(void *v, const Vix& x)
	    { return v != x.vix; }
	friend int operator!=(const Vix& x, void *v)
	    { return v != x.vix; }
	friend int operator==(const Vix& x1, const Vix& x2)
	    { return x1.owner == x2.owner && x1.vix == x2.vix; }
	friend int operator!=(const Vix& x1, const Vix& x2)
	    { return x1.owner != x2.owner || x1.vix != x2.vix; }
    private:
	friend class SetLS<T>;

	Vix(const SetLS<T> *o, const ListS<T>::Vix& x): owner(o), vix(x)
	    { }

	const SetLS<T> *owner;
	ListS<T>::Vix vix;
    };
    friend class Vix;
    
    Vix first() const
	{ return Vix(this, list.first()); }
    void next(Vix& x) const
	{ check(x);
	  list.next(x.vix); }
    const T& operator()(const Vix& x) const
	{ check(x);
	  return list(x.vix); }
    // There is item no remove(const Vix&) for this set
protected:
    ListS<T> list;

private:
    void check(const Vix& x) const
	{ if (this != x.owner)
	      ::abort(); }
};


template<class T>
SetLS<T>::SetLS():



list()

{ }

template<class T>
void
SetLS<T>::add(const T& item)
{
    if ( ! contains(item) ) {



	list.append(item);

    }
}

template<class T>
bool
SetLS<T>::contains(const T& item) const
{
    for (Vix x=first(); 0 != x; next(x)) {
	if (operator()(x) == item)
	    return XTRUE;
    }
    return XFALSE;
}


# 14 "SetLS.cc" 2



# 1 "/projects/gnu-cygnus/gnu-cygnus-14/mips/lib/gcc-lib/decstation/cygnus-reno-1/g++-include/iostream.h" 1 3
//    This is part of the iostream library, providing -*- C++ -*- input/output.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# 211 "/projects/gnu-cygnus/gnu-cygnus-14/mips/lib/gcc-lib/decstation/cygnus-reno-1/g++-include/iostream.h" 3

# 21 "SetLS.cc" 2


// In (most versions of) g++ 2.X, this use of typedefs has the effect
// of causing the instantiation of the templates, thereby testing the
// templates

class test {
public:
    test(): value(0)
	{ }
    test(int v): value(v)
	{ }

    void print(ostream& out) const
	{ out << value; }

    friend int operator==(const test& a, const test& b);
private:
    int value;
};

int
operator==(const test& a, const test& b)
{
    return a.value == b.value;
}

ostream&
operator<<(ostream& o, const test& t)
{
    t.print(o);
    return o;
}

typedef SetLS<test> SLS;

static ostream&
operator<<(ostream& o, const SLS& s)
{
    o << "set of " << s.length() << " = {";

    bool first;
    SetLS<test>::Vix x;
    for (first=XTRUE, x=s.first(); 0 != x; s.next(x), first=XFALSE) {
	if ( ! first )
	    o << ',';
	o << ' ';
	s(x).print(o);
    }
    o << '}';

    return o;
}

SLS gsls;
const SLS gcsls;

int foo()
{
    const unsigned SIZE = 20;

    //
    // SetLS()
    // SetLS(const SetLS<T>&)
    // 
    SLS sls;
    {
	// Fill sls with some interesting values
	for (unsigned i=0; i<SIZE; i++) {
	    test t = i;
	    sls.add(t);
	}
    }

    const SLS csls(sls);

    //
    // void operator=(const SetLS<T>&);
    //
    sls = csls;

    //
    // bool contains(const T& item) const
    //
    for (unsigned i=0; i<SIZE; i++) {
	test t = i;

	int contains = sls.contains(t);
    }

    //
    // void clear()
    //
    sls.clear();
    if (sls.length() != 0)
	::abort();

    sls = csls;

    //
    // Vix first() const
    // void next(Vix& x) const
    // T& operator()(const Vix& x)
    // const T& operator()(const Vix& x) const
    //
    SetLS<test>::Vix cx;
    for (cx=csls.first(); 0 != cx; sls.next(cx)) {
	if ( ! sls.contains(csls(cx)) )
	    ::abort();
    }

    cout << "gsls:\t" << gsls << '\n';
    cout << "gcsls:\t" << gcsls << '\n';
    cout << "sls:\t" << sls << '\n';
    cout << "csls:\t" << csls << '\n';
}

// Dummy function so it'll run
int main()
{
  cout << "PASS" << endl;
}

template class ListS<test>;
