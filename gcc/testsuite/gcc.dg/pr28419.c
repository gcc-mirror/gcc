/* { dg-do compile } */
void foo() 
const char* p = __FUNCTION__; /* { dg-error "" } */
