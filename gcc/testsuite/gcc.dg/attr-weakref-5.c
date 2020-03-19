/* PR middle-end/92799 - ICE on a weakref function definition followed
   by a declaration
   { dg-do compile }
   { dg-options "-Wall" } */

static __attribute__ ((weakref ("bar"))) void f0 (void) { }   // { dg-warning "'weakref' attribute ignored because function is defined" }

extern void f0 (void);

void* use_f0 (void) { return f0; }


static __attribute__ ((weakref ("bar"))) void f1 (void) { }   // { dg-warning "'weakref' attribute ignored because function is defined" }

static void f1 (void);

void* use_f1 (void) { return f1; }


static __attribute__ ((weakref ("bar"))) void f2 (void);

static void f2 (void) { }                                     // { dg-error "redefinition" }

void* use_f2 (void) { return f2; }


static __attribute__ ((weakref ("bar"))) void f3 (void);

void f3 (void) { }                                            // { dg-error "redefinition" }

void* use_f3 (void) { return f3; }
