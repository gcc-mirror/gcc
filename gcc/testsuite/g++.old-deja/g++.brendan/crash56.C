// Build don't link: 
// GROUPS passed old-abort
// Special g++ Options:

const bool FALSE = 0;
const bool TRUE = 1;
class ListDProto {
public:
    ListDProto();
    ListDProto(const ListDProto&);
    virtual ~ListDProto();
    void operator=(const ListDProto&);
    unsigned length() const;
    bool empty() const;
    void clear();
    void remove_head();
    void remove_tail();
    class link;
    class Vix {
    public:
	Vix();
	friend int operator==(void *v, const Vix& x)
	    { return v == x.item; }// ERROR - list of candidates
	friend int operator==(const Vix& x, void *v)
	    { return v == x.item; }// ERROR - candidate for call
	friend int operator!=(void *v, const Vix& x)
	    { return v != x.item; }
	friend int operator!=(const Vix& x, void *v)
	    { return v != x.item; }
	friend int operator==(const Vix& x1, const Vix& x2)
	    { return x1.owner == x2.owner && x1.item == x2.item; }// ERROR - candidate for call
	friend int operator!=(const Vix& x1, const Vix& x2)
	    { return x1.owner != x2.owner || x1.item != x2.item; }
	bool first;		 
	bool last;		 
    protected:
        friend class ListDProto;
	Vix(const ListDProto *o, link *i);
	const ListDProto *owner;
    private:
	link *item;
    };
    enum Action { NORMAL, REMOVE_CURRENT };
    Vix first() const;
    void first(Vix& x) const;
    void next(Vix& x) const;
    void next(Vix& x, Action a = NORMAL);
    Vix last() const;
    void last(Vix& x) const;
    void prev(Vix& x) const;
    void prev(Vix& x, Action a = NORMAL);
protected:
    struct link {
	link *next;
	link *prev;
	link(link *n = 0, link *p = 0);
	virtual ~link();
    private:
	link(const link&);
	void operator=(const link&);
    };
    unsigned count;
    link *list_head;		 
    link *list_tail;		 
    virtual link *copy_item(link *old_item) const = 0;
    void prepend(link *item);
    void append(link *item);
    void prepend(const ListDProto& proto);
    void append(const ListDProto& proto);
    void remove(link *item);
    link *ref(const Vix&) const;
};
template<class T>
class ListD: public ListDProto {
public:
    void prepend(const T& item);
    void append(const T& item);
    const T& head() const;
    T& head();
    void head(T& fill) const;
    void remove_head()
	{ ListDProto::remove_head(); }
    void remove_head(T& fill);
    const T& tail() const;
    T& tail();
    void tail(T& fill) const;
    void remove_tail()
	{ ListDProto::remove_tail(); }
    void remove_tail(T& fill);
    class Vix: public ListDProto::Vix {
    public:
	Vix(): ListDProto::Vix()
	    { }
    protected:
        friend class ListD<T>;
	Vix(const ListDProto::Vix& x): ListDProto::Vix(x)
	    { }
    };
    Vix first() const
	{ return ListDProto::first(); };
    void first(Vix& x) const
	{ ListDProto::first(x); };
    void next(Vix& x, ListDProto::Action a = NORMAL) const
	{ ListDProto::next(x, a); }// ERROR - .*// ERROR - .*
    Vix last() const
	{ return ListDProto::last(); }
    void last(Vix& x) const
	{ return ListDProto::last(x); }
    void prev(Vix& x, ListDProto::Action a = NORMAL) const
	{ return ListDProto::prev(x, a); }
protected:
    struct link_item: public ListDProto::link {
	T item;
	link_item(const T& i): link(0, 0), item(i)
	    { }
    private:
	link_item(const link_item&);
	void operator=(const link_item&);
    };
public:
    T& operator()(const Vix& x)
	{ link_item *li = (link_item *) ref(x);
	  return li->item; }
    const T& operator()(const Vix& x) const
	{ link_item *li = (link_item *) ref(x);
	  return li->item; }
private:
    ListDProto::link *copy_item(ListDProto::link *old_item) const;
};
template<class T>
class SetLD: private ListD<T> {
public:
    SetLD();			 
    SetLD(const ListD<T>&);	 
    void add(const T& item);
    void add(const ListD<T>& other);
    void add(const SetLD<T>& other);
    void remove(const T& item);
    bool contains(const T& item) const;
    ListD<T>::length;
    ListD<T>::empty;
    ListD<T>::clear;
    typedef ListD<T>::Vix Vix;
    ListD<T>::first;
    ListD<T>::next;
    ListD<T>::operator();
};
extern "C" {
extern void __eprintf (const char *, const char *, unsigned, const char *);
}
extern "C" {
extern void __eprintf (const char *, const char *, unsigned, const char *);
}
template<class T>
void
ListD<T>::prepend(const T& item)
{
    link *newl = new link_item(item);
    ListDProto::prepend(newl);
}
template<class T>
void
ListD<T>::append(const T& item)
{
    link *newl = new link_item(item); 
    ListDProto::append(newl);
}
template<class T>
const T&
ListD<T>::head() const
{
    ((void) (( 0 != list_head ) ? 0 : (__eprintf ("%s:%u: failed assertion `%s'\n",	  "/home/wbaker/work/include/templates/ListD.body.h" ,   50 ,  "0 != list_head" ), 0) )) ;
    link_item *h = (link_item *) list_head;
    return h->item;
}
template<class T>
T&
ListD<T>::head()
{
    ((void) (( 0 != list_head ) ? 0 : (__eprintf ("%s:%u: failed assertion `%s'\n",	  "/home/wbaker/work/include/templates/ListD.body.h" ,   59 ,  "0 != list_head" ), 0) )) ;
    link_item *h = (link_item *) list_head;
    return h->item;
}
template<class T>
void
ListD<T>::head(T& fill) const
{
    ((void) (( 0 != list_head ) ? 0 : (__eprintf ("%s:%u: failed assertion `%s'\n",	  "/home/wbaker/work/include/templates/ListD.body.h" ,   68 ,  "0 != list_head" ), 0) )) ;
    link_item *h = (link_item *) list_head;
    fill = h->item;
}
template<class T>
void
ListD<T>::remove_head(T& fill)
{
    head(fill);
    remove_head();
}
template<class T>
const T&
ListD<T>::tail() const
{
    ((void) (( 0 != list_tail ) ? 0 : (__eprintf ("%s:%u: failed assertion `%s'\n",	  "/home/wbaker/work/include/templates/ListD.body.h" ,   85 ,  "0 != list_tail" ), 0) )) ;
    link_item *h = (link_item *) list_tail;
    return h->item;
}
template<class T>
T&
ListD<T>::tail()
{
    ((void) (( 0 != list_tail ) ? 0 : (__eprintf ("%s:%u: failed assertion `%s'\n",	  "/home/wbaker/work/include/templates/ListD.body.h" ,   94 ,  "0 != list_tail" ), 0) )) ;
    link_item *h = (link_item *) list_tail;
    return h->item;
}
template<class T>
void
ListD<T>::tail(T& fill) const
{
    ((void) (( 0 != list_tail ) ? 0 : (__eprintf ("%s:%u: failed assertion `%s'\n",	  "/home/wbaker/work/include/templates/ListD.body.h" ,   103 ,  "0 != list_tail" ), 0) )) ;
    link_item *h = (link_item *) list_tail;
    fill = h->item;
}
template<class T>
void
ListD<T>::remove_tail(T& fill)
{
    ((void) (( 0 != list_tail ) ? 0 : (__eprintf ("%s:%u: failed assertion `%s'\n",	  "/home/wbaker/work/include/templates/ListD.body.h" ,   112 ,  "0 != list_tail" ), 0) )) ;
    link_item *h = (link_item *) list_tail;
    fill = h->item;
}
template<class T>
ListDProto::link *
ListD<T>::copy_item(ListDProto::link *old) const
{
    link_item *old_item = (link_item *) old;
    link_item *new_item = new link_item(old_item->item);
    return new_item;
}
template<class T>
SetLD<T>::SetLD():
ListD<T>()
{ }
template<class T>
SetLD<T>::SetLD(const ListD<T>& other):
ListD<T>(other)
{ }
template<class T>
void
SetLD<T>::add(const T& item)
{
    if ( ! contains(item) )
	append(item);
}
template<class T>
void
SetLD<T>::add(const ListD<T>& other)
{
    ListD<T>::Vix x;
    for (first(x); 0 != x; next(x))
	add(other(x));
}
template<class T>
void
SetLD<T>::add(const SetLD<T>& other)
{
    const ListD<T>& lother = other;
    add(lother);
}
template<class T>
void
SetLD<T>::remove(const T& item)
{
    Action a = NORMAL;
    Vix x;
    for (first(x); 0 != x && REMOVE_CURRENT != a; next(x, a))
	a = operator()(x) == item ? REMOVE_CURRENT: NORMAL;// ERROR - .*
}
template<class T>
bool
SetLD<T>::contains(const T& item) const
{
    Vix x;
    for (first(x); 0 != x; next(x)) {
	if (operator()(x) == item)// ERROR - .*
	    return TRUE;
    }
    return FALSE;
}
template<class T>
int
operator==(const SetLD<T>& a, const SetLD<T>& b)
{
    if (a.length() != b.length())
	return FALSE;
    SetLD<T>::Vix x;
    for (a.first(x); 0 != x; a.next(x)) {
	if ( ! b.contains(a(x)) )
	    return FALSE;
    }
    for (b.first(x); 0 != x; b.next(x)) {
	if ( ! a.contains(b(x)) )
	    return FALSE;
    }
    return TRUE;
}
template<class T>
int
operator!=(const SetLD<T>& a, const SetLD<T>& b)
{ return ! (a == b); }
template<class T>
int
operator<=(const SetLD<T>& a, const SetLD<T>& b)
{
    if (a.length() > b.length())
	return FALSE;
    SetLD<T>::Vix x;
    for (x=a.first(); 0 != x; a.next(x)) {
	if ( ! b.contains(a(x)) )
	    return FALSE;
    }
    return TRUE;
}
template<class T>
int
operator<(const SetLD<T>& a, const SetLD<T>& b)
{
    if (a.length() >= b.length())
	return FALSE;
    return a <= b;
}
template<class T>
int
operator>(const SetLD<T>& a, const SetLD<T>& b)
{ return ! (a <= b); }
template<class T>
int
operator>=(const SetLD<T>& a, const SetLD<T>& b)
{ return ! (a < b); }
class String { };
class IcaseString: public String { };
class SetLD< IcaseString >: public SetLD<    String  > {	public:	 SetLD (): SetLD<    String  >() { };	 SetLD (const ListD<   IcaseString  >& other): SetLD<    String  >()	{ ListD<   IcaseString  >::Vix x;	for (other.first(x); 0 != x; other.next(x))	add(other(x)); };	 SetLD (const  SetLD & other): SetLD<    String  >(other) { };	const    IcaseString  & operator()(const Vix& x) const	{ return (   IcaseString  &) SetLD<    String  >::operator()(x); }	}; 	typedef SetLD<  String > SetLD_String_IcaseString_old_tmp99;	typedef SetLD< IcaseString > SetLD_String_IcaseString_new_tmp99;	
inline int	 operator== (const SetLD_String_IcaseString_new_tmp99& a,	const SetLD_String_IcaseString_new_tmp99& b)
{// ERROR - candidate for call
const SetLD_String_IcaseString_old_tmp99& oa = a;
const SetLD_String_IcaseString_old_tmp99& ob = b;
return  operator== (oa, ob);	} 	
inline int	 operator!= (const SetLD_String_IcaseString_new_tmp99& a,	const SetLD_String_IcaseString_new_tmp99& b)
{
const SetLD_String_IcaseString_old_tmp99& oa = a;
const SetLD_String_IcaseString_old_tmp99& ob = b;
return  operator!= (oa, ob);	} 	
inline int	 operator< (const SetLD_String_IcaseString_new_tmp99& a,	const SetLD_String_IcaseString_new_tmp99& b)
{
const SetLD_String_IcaseString_old_tmp99& oa = a;
const SetLD_String_IcaseString_old_tmp99& ob = b;
return  operator< (oa, ob);	} 	
inline int	 operator<= (const SetLD_String_IcaseString_new_tmp99& a,	const SetLD_String_IcaseString_new_tmp99& b)
{
const SetLD_String_IcaseString_old_tmp99& oa = a;
const SetLD_String_IcaseString_old_tmp99& ob = b;
return  operator<= (oa, ob);	} 	
inline int	 operator> (const SetLD_String_IcaseString_new_tmp99& a,	const SetLD_String_IcaseString_new_tmp99& b)
{
const SetLD_String_IcaseString_old_tmp99& oa = a;
const SetLD_String_IcaseString_old_tmp99& ob = b;
return  operator> (oa, ob);	} 	
inline int	 operator>= (const SetLD_String_IcaseString_new_tmp99& a,	const SetLD_String_IcaseString_new_tmp99& b)
{
const SetLD_String_IcaseString_old_tmp99& oa = a;
const SetLD_String_IcaseString_old_tmp99& ob = b;
return  operator>= (oa, ob);	}   ;
typedef SetLD<IcaseString> SLDiS;
static void
nop(int i)
{
    SetLD<IcaseString> x, y;
    nop(x == y);
 nop(x != y);
nop(x < y);
nop(x <= y);
nop(x > y);
nop(x >= y);
}

template class SetLD<String>;
