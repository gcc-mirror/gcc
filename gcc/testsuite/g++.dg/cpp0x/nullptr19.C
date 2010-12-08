// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test overload of pointer versus nullptr_t when applied on a literal 0/__null

typedef decltype(nullptr) nullptr_t;

char* k( char* );	/* { dg-message "note" } { dg-message "note" } */
nullptr_t k( nullptr_t ); /* { dg-message "note" } { dg-message "note" } */

void test_k()
{
  k(0); /* { dg-error "is ambiguous" } */
  // { dg-message "candidate" "candidate note" { target *-*-* } 13 }
  k(__null); /* { dg-error "is ambiguous" } */
  // { dg-message "candidate" "candidate note" { target *-*-* } 15 }
}
