// Build don't link: 
// GROUPS passed nested-classes
class BDDHeap {
public:
    BDDHeap();
    BDDHeap(const BDDHeap&);

    class Page {
    public:
	int i;
    };
    struct Pointer {
	Page *page;
	unsigned index;

	Pointer();
	Pointer(const BDDHeap& heap);
    };

    struct Half {
	struct {
	    Page *top;
	    Page **tail;
	} inuse;

	Half();
    };

    Half half[2];
    unsigned halfspace;
};

inline
BDDHeap::Pointer::Pointer(const BDDHeap& heap):
page(0),
index(0)
{
    Page *x = heap.half[heap.halfspace].inuse.top;
    page = x;
}

