// Build don't link:
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Nov 2000 <nathan@codesourcery.com>

// Bug 573. We ICE'd verifying operator new and operator delete conformed
// to the standard's expectation.

void *operator new (__SIZE_TYPE__); // ok
void operator new (__SIZE_TYPE__);  // ERROR - must return void *
void *operator new ();              // ERROR - must take size_t
void *operator new (char);          // ERROR - must take size_t
void *operator new (__SIZE_TYPE__, ...) throw(); // ok

void operator delete (void *) throw (); // ok
int operator delete (void *) throw ();          // ERROR - must return void
void operator delete () throw ();               // ERROR - must take void *
void operator delete (int *) throw ();          // ERROR - must take void *
void operator delete (void *, __SIZE_TYPE__) throw (); // ok

void operator delete (...) throw ();             // ERROR - must take void *
void operator delete (void *, ...) throw ();     // ok
