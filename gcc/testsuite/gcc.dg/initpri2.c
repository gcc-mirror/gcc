/* { dg-do compile { target init_priority } } */

/* Priorities must be in the range [0, 65535].  */
void c1()
     __attribute__((constructor (-1))); /* { dg-error "priorities" } */
void c2() 
     __attribute__((constructor (65536))); /* { dg-error "priorities" } */
void d1() 
     __attribute__((destructor (-1))); /* { dg-error "priorities" } */
void d2() 
     __attribute__((destructor (65536))); /* { dg-error "priorities" } */

/* Priorities 0-100 are reserved for system libraries.  */
void c3() 
     __attribute__((constructor (50))); /* { dg-warning "reserved" } */
void d3() 
     __attribute__((constructor (50))); /* { dg-warning "reserved" } */

