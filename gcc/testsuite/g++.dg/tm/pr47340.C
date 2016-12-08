// { dg-do compile }
// { dg-options "-fgnu-tm" }

void* operator new(__SIZE_TYPE__)
#if __cplusplus <= 201402L
throw (int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
;

void *point;

void funky()
{
  point = new (int);
}
