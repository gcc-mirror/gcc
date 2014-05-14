// { dg-do compile { target c++11 } }


// Test overload of pointer versus nullptr_t when applied on a literal 0/__null

typedef decltype(nullptr) nullptr_t;

char* k( char* );	/* { dg-message "note" } */
nullptr_t k( nullptr_t ); /* { dg-message "note" } */

void test_k()
{
  k(0); /* { dg-error "is ambiguous" } */
  k(__null); /* { dg-error "is ambiguous" } */
}
