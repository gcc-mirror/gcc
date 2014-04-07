// { dg-do assemble  }
// { dg-options "-pedantic-errors" }
// GROUPS passed initialization
// init file
// From: dcb@us-es.sel.de
// Date:     Mon, 12 Oct 92 07:51:52 +0100
// Subject:  G++, ARM Page 141
// Message-ID: <9210120651.AA15257@us-es.sel.de>

extern int fred( int);

class X {
      public :
      void f( int = fred( 0) ) ; // { dg-message "previous spec" }
} ;

void X::f( int x = fred( 0) ) { // { dg-error "default argument" }
}
