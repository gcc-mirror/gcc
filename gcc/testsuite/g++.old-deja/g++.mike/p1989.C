// Build don't link:
// prms-id: 1989

#define TRUE true
#define FALSE false
typedef void *Pix;

template<class T>
struct link {
    T item;
    link *next;
    link *prev;

    link(const T& t): item(t), prev(0), next(0)
	{ };
    link(const T& t, link<T> *p, link<T> *n): item(t), prev(p), next(n)
	{ };
};

template<class T>
class List_DL {
public:
    List_DL();
    List_DL(const List_DL&);
    ~List_DL();

    void append(const T& item);
    void prepend(const T& item);
    void insert(const T& item, Pix x, bool before);

    void remove(Pix& x)
	{ T tmp; remove(x, tmp); }
    void remove(Pix& x, T& item);

    void clear();

    unsigned length() const
	{ return count; }

private:

    unsigned count;
    link<T> *head;
    link<T> *tail;

public:
    Pix first() const
	{ return Pix(head); }
    Pix last() const
	{ return Pix(tail); }
    void next(Pix& x) const
	{ if (0 != x) x = ((link<T> *) x)->next; }
    void prev(Pix& x) const
	{ if (0 != x) x = ((link<T> *) x)->prev; }
    T& operator()(Pix x) const
	{ return ((link<T> *) x)->item; }
};

template<class T>
List_DL<T>::List_DL():
count(0),
head(0)
{ }

template<class T>
List_DL<T>::List_DL(const List_DL& other):
count(0),
head(0)
{
    for (Pix x=other.first(); 0 != x; other.next(x))
	append(other(x));
}

template<class T>
List_DL<T>::~List_DL()
{
    clear();
}

template<class T>
void
List_DL<T>::append(const T& item)
{
    count++;
    if (0 == head) {
	head = new link<T>(item);
	tail = head;
    } else {
	tail->next = new link<T>(item, tail, 0);
	tail = tail->next;
    }
}

template<class T>
void
List_DL<T>::prepend(const T& item)
{
    count++;
    if (0 == head) {
	head = new link<T>(item);
	tail = head;
    } else {
	head = new link<T>(item, 0, head);
	if (tail == head)
	    tail = tail->next;
    }
}

template<class T>
void
List_DL<T>::insert(const T& item, Pix x, bool before = TRUE)
{
    link<T> *l = (link<T> *) x;

    if (before) {
	if (0 == l || l == head) {
	    prepend(item);
	} else {
	    link<T> *n = new link<T>(item, l->prev, l);
	    l->prev->next = n;
	    l->prev = n;
	}
    } else {
	if (0 == l || l == tail) {
	    append(item);
	} else {
	    link<T> *n = new link<T>(item, l, l->next);
	    l->next->prev = n;
	    l->prev = n;
	}
    }
}

template<class T>
void
List_DL<T>::remove(Pix& x, T& item)
{
    link<T> *l = (link<T> *) x;

    if (0 == l)
	return;

    item = l->item;
    if (1 == count) {
	delete head;
	head = 0;
	tail = 0;
    } else {
	// more than one item in the list
	if (l == head) {
	    link<T> *old = head;
	    head = head->next;
	    head->prev = 0;
	    delete old;
	} else if (l == tail) {
	    link<T> *old = tail;
	    tail = tail->prev;
	    tail->next = 0;
	    delete old;
	} else {
	    l->next->prev = l->prev;
	    l->prev->next = l->next;
	    delete l;
	}
    }
}

template<class T>
void
List_DL<T>::clear()
{
    link<T> *l, *succ;
    for (l=head; 0 != l; l=succ) {
	succ = l->next;
	delete l;
    }
    head = 0;
    tail = 0;
}

template<class T>
class List_DLS: public List_DL<T> {
public:
    List_DLS(): List_DL<T>()
	{ };
    List_DLS(const List_DLS& other): List_DL<T>(other)
	{ };

    bool contains(const T& item) const
	{ return search(item) != 0 ? TRUE: FALSE; }
    Pix search(const T&) const;
};

template<class T>
Pix
List_DLS<T>::search(const T& item) const
{
    for (Pix x=first(); 0 != x; next(x)) {
	if (item == operator()(x)) // ERROR - const subversion
	    return x;
    }
    return 0;
}

template<class T>
class List_DLSp: public List_DL<T> {
public:
    List_DLSp(): List_DL<T>()
	{ };
    List_DLSp(const List_DLSp& other): List_DL<T>(other)
	{ };

    bool contains(const T& item) const
#ifndef INTERNAL_ERROR
	;
#else
	{ return search(item) != 0 ? TRUE: FALSE; }
#endif
    Pix search(const T&) const;
};

template<class T>
bool
List_DLSp<T>::contains(const T& item) const
{
    for (Pix x=first(); 0 != x; next(x)) {
	if (*item == *operator()(x))
	    return TRUE;
    }
    return FALSE;
}

template<class T>
class Set {
public:
    Set();
    Set(const Set& other);

    virtual void add(const T& item);

    void remove(const T& item)
	{ Pix x = search(item); remove(x); }
    void remove(Pix& x)
	{ T tmp; remove(x, tmp); }
    virtual void remove(Pix& x, T& item);

    virtual void clear();

    virtual bool contains(const T&) const;
    virtual Pix search(const T&) const;

    virtual unsigned length() const;

    virtual Pix first() const;
    virtual void next(Pix& x) const;
    virtual T& operator()(Pix x) const;
};

template<class T>
Set<T>::Set()
{ }

template<class T>
Set<T>::Set(const Set& other)
{ }


template<class T>
class Set_DL: public List_DLS<T> {
public:
    Set_DL();
    Set_DL(const Set_DL& other);

    void add(const T& item)
	{ list.append(item); }
    void remove(Pix& x, T& item)
	{ list.remove(x, item); }

    void clear()
	{ list.clear(); }

    bool contains(const T& item) const
	{ return list.contains(item); }
    Pix search(const T& item) const
	{ return list.search(item); }

    unsigned length() const
	{ return list.length(); }

    Pix first() const
	{ return list.first(); }
    void next(Pix& x) const
	{ list.next(x); }
    T& operator()(Pix x) const
	{ return list(x); }
private:
    List_DLS<T> list;
};

template<class T>
class Set_DLp: public List_DLSp<T> {
public:
    Set_DLp();
    Set_DLp(const Set_DLp& other);

    void add(const T& item)
	{ list.append(item); }
    void remove(Pix& x, T& item)
	{ list.remove(x, item); }

    void clear()
	{ list.clear(); }

    bool contains(const T& item) const
	{ return list.contains(item); }
    Pix search(const T& item) const
	{ return list.search(item); }

    unsigned length() const
	{ return list.length(); }

    Pix first() const
	{ return list.first(); }
    void next(Pix& x) const
	{ list.next(x); }
    T& operator()(Pix x) const
	{ return list(x); }
private:
    List_DLSp<T> list;
};

template<class T>
struct vertex {
    T item;
    List_DL<vertex<T> *> fanout;

    vertex(): item(), fanout()	// gets bogus error
      { };
    vertex(const T& i): item(), fanout() // gets bogus error
      { };
};

template<class T>
class Graph {
public:
    Graph();
    Graph(const Graph&);
    ~Graph();

    void add(const T& from, const T& to);
    bool contains(const T& from, const T& to) const;

    void clear()
	{ vertices.clear(); }

    unsigned lengthV() const
	{ return vertices.length(); }

    Pix firstV() const
	{ return vertices.first(); }
    void nextV(Pix& x) const
	{ vertices.next(x); }
    T& V(Pix x) const
	{ return vertices(x).item; }

    Pix firstV1(Pix vx) const;
    void nextV1(Pix vx, Pix& x) const;
    T& V1(Pix vx, Pix x) const;
private:
    vertex<T> *lookup(const T& from) const;
    vertex<T> *lookup_new(const T& from);

    List_DLS<vertex<T> > vertices;
};

template<class T>
Graph<T>::Graph():
vertices()
{ }

template<class T>
Graph<T>::Graph(const Graph& other):
vertices()
{
    for (Pix vx=firstV(); 0 != vx; nextV(vx)) {
	for (Pix vx1=firstV1(vx); 0 != vx1; nextV1(vx, vx1)) {
	    add(V(vx), V1(vx, vx1));
	}
    }
}

template<class T>
Graph<T>::~Graph()
{
    clear();
}

template<class T>
void
Graph<T>::add(const T& from, const T& to)
{
    vertex<T> *fromv = lookup_new(from);
    if (from == to)
	return;
    vertex<T> *tov = lookup_new(to);
    fromv->fanout.append(tov);
}

template<class T>
bool
Graph<T>::contains(const T& from, const T& to) const
{
    vertex<T> *fromv = lookup(from);
    if (0 == fromv)
	return FALSE;

    for (Pix x=fromv->fanout.first(); 0 != x; fromv->fanout.next(x)) {
	if (fromv->fanout(x)->item == to)
	    return TRUE;
    }

    return FALSE;
}

template<class T>
vertex<T> *
Graph<T>::lookup(const T& from) const
{
    for (Pix x=vertices.first(); 0 != x; vertices.next(x)) {
	if (vertices(x).item == from)
	    return &vertices(x);
    }
    return 0;
}

template<class T>
vertex<T> *
Graph<T>::lookup_new(const T& from)
{
    vertex<T> *v = lookup(from);
    if (0 == v) {
	vertices.append(from);
	return &vertices(vertices.last());
    }
    return v;
}

template<class T>
Pix
Graph<T>::firstV1(Pix vx) const
{
    vertex<T> *v = (vertex<T> *) vx;
    return v->fanout.first();
}

template<class T>
void
Graph<T>::nextV1(Pix vx, Pix& x) const
{
    vertex<T> *v = (vertex<T> *) vx;
    return v->fanout.next(x);
}

template<class T>
T&
Graph<T>::V1(Pix vx, Pix x) const
{
    vertex<T> *v = (vertex<T> *) vx;
    static T x1;
    return x1;
}

class STRLIdentifier;

extern int x(List_DL<STRLIdentifier *>);
extern int x(List_DLS<STRLIdentifier *>);

extern int x(Set<STRLIdentifier *>);
extern int x(Set_DL<STRLIdentifier *>);
extern int x(Set_DLp<STRLIdentifier *>);

extern int x(Graph<STRLIdentifier *>);

class STRLIdentifier {
    char buf[10];
};

extern int operator==(vertex<STRLIdentifier*>&, vertex<STRLIdentifier*>&); // ERROR - const subversion
extern int operator==(STRLIdentifier&, STRLIdentifier&); // ERROR - fn ref in err msg

extern int x(List_DLSp<STRLIdentifier *>);

template class Graph<STRLIdentifier *>;
template class List_DLS<vertex<STRLIdentifier *> >;
