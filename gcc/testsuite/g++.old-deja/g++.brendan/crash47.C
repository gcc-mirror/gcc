// Build don't link: 
// GROUPS passed old-abort
const int TRUE = 1;
const int FALSE = 0;

class Rep {
protected:
    Rep(): count(0)
	{ }
    Rep(const Rep& other): count(0)
	{ }

    Rep& operator=(const Rep& other)
	{ /* DO NOT copy over other.count */
	  return *this; }

public:		// TODO - for now
    // Because it is to hard to restrict these operations to the descendants
    // of Rep<REP> that we haven't named yet.  So we just make them public.
    void inc()
	{ count++; }
    void dec()
	{ if (0 == --count) delete this; }
private:
    unsigned count;
};

template<class REP>
class Ref {
public:
    Ref(): rep(0)
	{ }
    Ref(const Ref<REP>& other): rep(other.rep)
	{ if (rep) rep->inc(); }
    ~Ref()
	{ if (rep) rep->dec();
	  rep = 0; }

    Ref<REP>& operator=(const Ref<REP>& other)
	{ if (rep != other.rep) {
	    if (rep) rep->dec();
	    rep = other.rep;
	    if (rep) rep->inc(); }
	  return *this; }

    bool null() const
	{ return 0 == rep ? TRUE: FALSE; }
    bool valid() const
	{ return 0 != rep ? TRUE: FALSE; }

    REP* operator->() const		// should be a valid() reference
	{ return rep; }
    operator REP*() const;		// should be a valid() reference

protected:
    REP *rep;

    Ref(REP *r): rep(r)
	{ if (rep) rep->inc(); }

    Ref<REP>& operator=(REP *r)
	{ if (rep != r) {
	    if (rep) rep->dec();
	    rep = r;
	    if (rep) rep->inc(); }
	  return *this; }
};

template<class REP>
Ref<REP>::operator REP*() const		// should be a valid() reference
{ return rep; }

template<class REP> 
inline int
operator==(const Ref<REP>& a, const Ref<REP>& b)
{ return (REP *) a == (REP *) b; }

template<class REP> 
inline int
operator!=(const Ref<REP>& a, const Ref<REP>& b)
{ return (REP *) a != (REP *) b; }

class XRep: public Rep {
public:
    int i;
};

int
main()
{
    Ref<XRep> y;

    return y != y;
}
