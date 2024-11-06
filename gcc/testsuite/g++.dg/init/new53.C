// Check that we reject operator new with no argument or non-size_t first
// argument.
// { dg-do "compile" }

void* operator new(); // { dg-error "takes type .size_t." }
void* operator new(char); // { dg-error "takes type .size_t." }
void* operator new(char*); // { dg-error "takes type .size_t." }
void* operator new(char&); // { dg-error "takes type .size_t." }
