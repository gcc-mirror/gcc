/* PR middle-end/94527 - Add an attribute that marks a function as freeing
   an object
   { dg-do compile { target c++11 } }
   { dg-options "-Wall" } */

#define A(...) __attribute__ ((malloc (__VA_ARGS__)))

typedef __SIZE_TYPE__ size_t;

void                   mydealloc (int, void*);
void* A (mydealloc, 2) myalloc (void*);


void* A (operator delete, 1)
  bad_new (size_t);                     // { dg-error "attribute argument 1 is ambiguous" "" { target c++14 } }
void* A (operator delete[], 1)
  bad_array_new (size_t);               // { dg-error "attribute argument 1 is ambiguous" "" { target c++14 } }

void my_delete (const char*, void*);
void my_array_delete (const char*, void*);

typedef void OpDelete (void*);

int* A ((OpDelete*)operator delete, 1) A (my_delete, 2)
  my_new (size_t);
int* A ((OpDelete*)operator delete[], 1) A (my_array_delete, 2)
  my_array_new (size_t);
