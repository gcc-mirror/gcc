/* { dg-options "" } */
/* { dg-do compile } */
//  Testing if we can do a new of a complex type
// PR C++/28450

void* q = new __complex__ int ();
