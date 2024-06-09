/* { dg-do compile { target init_priority } } */
/* Via the magic string "-std=*++" indicate that testing one (the default) C++ standard is sufficient.  */

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

/* Priorities must be integral constants.  */

/* Pointers, even with constant values, are not allowed.  */
void c4() 
     __attribute__((constructor ((void*) 500))); /* { dg-error "priorities" } */
void d4()    
     __attribute__((destructor ((void*) 500))); /* { dg-error "priorities" } */

/* Integer variables are not allowed.  */
int i;
void c5() 
     __attribute__((constructor ((i)))); /* { dg-error "priorities" } */
void d5()    
     __attribute__((destructor ((i)))); /* { dg-error "priorities" } */

/* Enumeration constants are allowed.  */
enum E { e = 500 };
void c6() 
     __attribute__((constructor ((e))));
void d6()    
     __attribute__((destructor ((e))));
