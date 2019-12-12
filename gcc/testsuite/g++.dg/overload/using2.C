// { dg-do compile }

// Copyright 2005 Free Software Foundation
// by Alexandre Oliva <aoliva@redhat.com>
// based on https://bugzilla.redhat.com/beta/show_bug.cgi?id=149098

// Per the ISO C++ 90 Standard, using declarations before of after a
// declaration of the same function name and prototype should be
// errors (7.3.3/11).  However, DR 101's resolution recommends
// accepting such duplicates if they denote the same function, which
// means extern "C" declarations are supposed to match and be
// accepted.

// This test makes sure we reject or accept regular and using
// declarations regardless of order as appropriate, and that having
// built-in declarations or overloads doesn't affet the outcome.

namespace std {
  extern "C" void exit (int) throw (); // these are built-in (extern "C")
  extern "C" void *malloc (__SIZE_TYPE__) throw () __attribute__((malloc));

  void abort (void) throw (); // these aren't
  void _exit (int) throw (); // { dg-message "std::_exit" }

  extern "C" void c1 (void) throw ();
  void C1 (void) throw (); // { dg-message "std::C1" }

  extern "C" void c2 (void) throw ();
  void C2 (void) throw ();

  extern "C" void c3 (void) throw ();
  void C3 (void) throw (); // { dg-message "std::C3" }
}

namespace other {
  extern "C" void c3 (void) throw ();
  void C3 (void) throw (); // { dg-message "other::C3" }
}

using std::exit;
using std::_exit;
using std::c1;
using std::C1;

  extern "C" void exit (int) throw ();
  extern "C" void *malloc (__SIZE_TYPE__) throw () __attribute__((malloc));

  void abort (void) throw (); // { dg-message "previous" }
  void _exit (int) throw (); // { dg-error "conflicts" "conflicts" }
                             // { dg-message "void _exit" "_exit" { target *-*-* } .-1 }

  extern "C" void c1 (void) throw ();
  void C1 (void) throw (); // { dg-error "conflicts" "conflicts" }
                           // { dg-message "void C1" "C1" { target *-*-* } .-1 }

  extern "C" void c2 (void) throw ();
  void C2 (void) throw (); // { dg-message "previous" }

  int C3 (int) throw ();

using std::malloc;
using std::abort; // { dg-error "conflicts" }
using std::c2;
using std::C2; // { dg-error "conflicts" }

using std::c3; using other::c3;
using std::C3; using other::C3;

  long C3 (long) throw ();

int main () {
  void *p = malloc (0);
  exit (0);

  _exit (0); // { dg-error "ambiguous" }
  abort ();

  c1 ();
  C1 (); // { dg-error "ambiguous" }

  c2 ();
  C2 (); // one might expect an ambiguous call error here as well, but
	 // we don't add the using decl if we find it to be in error.

  c3 ();
  C3 (); // { dg-error "ambiguous" }
  C3 (0);
  C3 (0l);
}
