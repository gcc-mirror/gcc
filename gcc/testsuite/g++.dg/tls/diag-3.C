// Report invalid extern and __thread combinations.
// { dg-require-effective-target tls }

extern int j;		// { dg-message "previously declared here" }
__thread int j;		// { dg-error "follows non-thread-local" }

extern __thread int i;	// { dg-message "previously declared here" }
int i;			// { dg-error "follows thread-local" }

extern __thread int k;	// This is fine.
__thread int k;
