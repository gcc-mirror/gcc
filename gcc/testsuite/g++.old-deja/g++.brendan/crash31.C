// Build don't link: 
// GROUPS passed old-abort
struct mbuf {
	struct mbuf *next;
};

void* malloc(int);
struct mbuf *
mbuf_allocate(int size)
{
    struct mbuf *bp;

    bp = (struct mbuf *) malloc( 10 /*(unsigned) (size + sizeof(struct mbuf))*/);
    return bp;

}
