/*
 * This is a reimplementation of a subset of the pthread_getspecific/setspecific
 * interface. This appears to outperform the standard linuxthreads one
 * by a significant margin.
 * The major restriction is that each thread may only make a single
 * pthread_setspecific call on a single key.  (The current data structure
 * doesn't really require that.  The restriction should be easily removable.)
 * We don't currently support the destruction functions, though that
 * could be done.
 * We also currently assume that only one pthread_setspecific call
 * can be executed at a time, though that assumption would be easy to remove
 * by adding a lock.
 */

#include <errno.h>

/* Called during key creation or setspecific.		*/
/* For the GC we already hold lock.			*/
/* Currently allocated objects leak on thread exit.	*/
/* That's hard to fix, but OK if we allocate garbage	*/
/* collected memory.					*/
#define MALLOC_CLEAR(n) GC_INTERNAL_MALLOC(n, NORMAL)
#define PREFIXED(name) GC_##name

#define TS_CACHE_SIZE 1024
#define CACHE_HASH(n) (((((long)n) >> 8) ^ (long)n) & (TS_CACHE_SIZE - 1))
#define TS_HASH_SIZE 1024
#define HASH(n) (((((long)n) >> 8) ^ (long)n) & (TS_HASH_SIZE - 1))

typedef struct thread_specific_entry {
	unsigned long qtid;	/* quick thread id, only for cache */
	void * value;
	pthread_t thread;
	struct thread_specific_entry *next;
} tse;


/* We represent each thread-specific datum as two tables.  The first is	*/
/* a cache, index by a "quick thread identifier".  The "quick" thread	*/
/* identifier is an easy to compute value, which is guaranteed to	*/
/* determine the thread, though a thread may correspond to more than	*/
/* one value.  We typically use the address of a page in the stack.	*/
/* The second is a hash table, indexed by pthread_self().  It is used	*/
/* only as a backup.							*/

/* Return the "quick thread id".  Default version.  Assumes page size,	*/
/* or at least thread stack separation, is at least 4K.			*/
static __inline__ long quick_thread_id() {
    int dummy;
    return (long)(&dummy) >> 12;
}

#define INVALID_QTID ((unsigned long)(-1))

typedef struct thread_specific_data {
    tse * volatile cache[TS_CACHE_SIZE];
			/* A faster index to the hash table */
    tse * hash[TS_HASH_SIZE];
    pthread_mutex_t lock;
} tsd;

typedef tsd * PREFIXED(key_t);

extern int PREFIXED(key_create) (tsd ** key_ptr, void (* destructor)(void *));

extern int PREFIXED(setspecific) (tsd * key, void * value);

extern void PREFIXED(remove_specific) (tsd * key);

/* An internal version of getspecific that assumes a cache miss.	*/
void * PREFIXED(slow_getspecific) (tsd * key, unsigned long qtid,
				   tse * volatile * cache_entry);

static __inline__ void * PREFIXED(getspecific) (tsd * key) {
    long qtid = quick_thread_id();
    unsigned hash_val = CACHE_HASH(qtid);
    tse * volatile * entry_ptr = key -> cache + hash_val;
    tse * entry = *entry_ptr;   /* Must be loaded only once.	*/
    if (entry -> qtid == qtid) return entry -> value;
    return PREFIXED(slow_getspecific) (key, qtid, entry_ptr);
}


