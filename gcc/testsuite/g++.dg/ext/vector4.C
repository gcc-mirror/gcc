/* { dg-options "" } */
/* { dg-do compile } */
//  Testing if we can do a new of a vector
// PR C++/28450

void* q = new int __attribute__((vector_size(8))) ();
