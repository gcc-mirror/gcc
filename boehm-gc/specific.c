/* 
 * Copyright (c) 2000 by Hewlett-Packard Company.  All rights reserved.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */

#if defined(LINUX_THREADS) || defined(GC_LINUX_THREADS)

#include "private/gc_priv.h" /* For GC_compare_and_exchange, GC_memory_barrier */
#include "private/specific.h"

static tse invalid_tse; 	/* 0 qtid is guaranteed to be invalid	*/

int PREFIXED(key_create) (tsd ** key_ptr, void (* destructor)(void *)) {
    int i;
    tsd * result = (tsd *)MALLOC_CLEAR(sizeof (tsd));

    if (0 == result) return ENOMEM;
    pthread_mutex_init(&(result -> lock), NULL);
    for (i = 0; i < TS_CACHE_SIZE; ++i) {
	result -> cache[i] = &invalid_tse;
    }
    *key_ptr = result;
    return 0;
}

int PREFIXED(setspecific) (tsd * key, void * value) {
    pthread_t self = pthread_self();
    int hash_val = HASH(self);
    volatile tse * entry = (volatile tse *)MALLOC_CLEAR(sizeof (tse));
    
    if (0 == entry) return ENOMEM;
    pthread_mutex_lock(&(key -> lock));
    /* Could easily check for an existing entry here.	*/
    entry -> next = key -> hash[hash_val];
    entry -> thread = self;
    entry -> value = value;
    /* There can only be one writer at a time, but this needs to be	*/
    /* atomic with respect to concurrent readers.			*/ 
    *(volatile tse **)(key -> hash + hash_val) = entry;
    pthread_mutex_unlock(&(key -> lock));
    return 0;
}

/* Remove thread-specific data for this thread.  Should be called on	*/
/* thread exit.								*/
void PREFIXED(remove_specific) (tsd * key) {
    pthread_t self = pthread_self();
    unsigned hash_val = HASH(self);
    tse *entry;
    tse **link = key -> hash + hash_val;

    pthread_mutex_lock(&(key -> lock));
    entry = *link;
    while (entry != NULL && entry -> thread != self) {
	link = &(entry -> next);
        entry = *link;
    }
    /* Invalidate qtid field, since qtids may be reused, and a later 	*/
    /* cache lookup could otherwise find this entry.			*/
        entry -> qtid = INVALID_QTID;
    if (entry != NULL) {
	*link = entry -> next;
	/* Atomic! concurrent accesses still work.	*/
	/* They must, since readers don't lock.		*/
    }
    /* If we wanted to deallocate the entry, we'd first have to clear 	*/
    /* any cache entries pointing to it.  That probably requires	*/
    /* additional synchronization, since we can't prevent a concurrent 	*/
    /* cache lookup, which should still be examining deallocated memory.*/
    /* This can only happen if the concurrent access is from another	*/
    /* thread, and hence has missed the cache, but still...		*/

    /* With GC, we're done, since the pointers from the cache will 	*/
    /* be overwritten, all local pointers to the entries will be	*/
    /* dropped, and the entry will then be reclaimed.			*/
    pthread_mutex_unlock(&(key -> lock));
}

/* Note that even the slow path doesn't lock.	*/
void *  PREFIXED(slow_getspecific) (tsd * key, unsigned long qtid,
				    tse * volatile * cache_ptr) {
    pthread_t self = pthread_self();
    unsigned hash_val = HASH(self);
    tse *entry = key -> hash[hash_val];

    while (entry != NULL && entry -> thread != self) {
	entry = entry -> next;
    } 
    if (entry == NULL) return NULL;
    /* Set cache_entry.		*/
        entry -> qtid = qtid;
		/* It's safe to do this asynchronously.  Either value 	*/
		/* is safe, though may produce spurious misses.		*/
	*cache_ptr = entry;
		/* Again this is safe since pointer assignments are 	*/
		/* presumed atomic, and either pointer is valid.	*/
    return entry -> value;
}

#endif /* LINUX_THREADS */
