// { dg-do assemble }

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Sep 1999 <nathan@acm.org>

// [over.match] 13.3 tells us where overload resolution occurs.
// [over.match.call] 13.3.1.1 says that in
//  (...( postfix-expression )...) (expression-list)
// the postfix-expression must be the name of a function (amongst some other
// choices). This means comma and conditional exprs cannot be placed there.
// This clause is the only one I can find which bans
//  (cond ? fna : fnb) (arglist)
// which would be a major headache to have to implement.
// [over.over] 13.4 tells us when the use of a function name w/o arguments is
// resolved to the address of a particular function. These are determined by
// the context of the function name, and it does allow more complicated primary
// expressions.

// Using a naked function name is rather strange, we used to warn about it
// (rather inconsistently), but subsequent changes broke the warning. Make
// sure that doesn't happen again.


void ovl (int);          // { dg-error "" } candidate
// { dg-message "int" "int" { target *-*-* } 24 }
void ovl (float);        // { dg-error "" } candidate
// { dg-message "float" "float" { target *-*-* } 26 }
void fn (int);
void fna (int);

int main (int argc, char **argv)
{
  void (*ptr) (int);
  void (*vptr) ();
  
  (ovl) (1);                // ok
  (&ovl) (1);               // { dg-error "" } not suitable for overload resolution
  (ovl) ();                 // { dg-error "" } no matching candidates
  (&ovl) ();                // { dg-error "" } not suitable for overload resolution
  
  // 13.3.1.1 indicates that the following are errors -- the primary expression
  // is not the name of a function.
  (0, ovl) (1);             // { dg-error "" } not suitable for overload resolution
  (0, &ovl) (1);            // { dg-error "" } not suitable for overload resolution
  (argc ? ovl : ovl) (1);   // { dg-error "" } not suitable for overload resolution
  (argc ? &ovl : &ovl) (1); // { dg-error "" } not suitable for overload resolution
  
  (fn) (1);                 // ok
  (&fn) (1);                // ok (no overload resolution)
  (0, fn) (1);              // ok (no overload resolution)
  (0, &fn) (1);             // ok (no overload resolution)
  (argc ? fna : fn) (1);    // ok (no overload resolution)
  (argc ? &fna : &fn) (1);  // ok (no overload resolution)
  
  ptr = (ovl);              // ok
  ptr = (&ovl);             // ok
  // 13.4 indicates these are ok.
  ptr = (0, ovl);           // ok { dg-bogus "" "" { xfail *-*-* } }
  ptr = (0, &ovl);          // ok { dg-bogus "" "" { xfail *-*-* } }
  ptr = (argc ? ovl : ovl); // ok { dg-bogus "" "" { xfail *-*-* } }
  ptr = (argc ? &ovl : &ovl);// ok { dg-bogus "" "" { xfail *-*-* } }
  
  vptr = (ovl);              // { dg-error "" } no matching candidates
  vptr = (&ovl);             // { dg-error "" } no matching candidates
  vptr = (0, ovl);           // { dg-error "" } no matching candidates
  vptr = (0, &ovl);          // { dg-error "" } no matching candidates
  vptr = (argc ? ovl : ovl); // { dg-error "" } no matching candidates
  vptr = (argc ? &ovl : &ovl);// { dg-error "" } no matching candidates
  
  ptr = (fn);
  ptr = (&fn);
  ptr = (0, fn);
  ptr = (0, &fn);
  ptr = (argc ? fna : fn);
  ptr = (argc ? &fna : &fn);
  
  f;                // { dg-error "" } not a call
  ovl;              // { dg-error "" } not suitable for overload
  &ovl;             // { dg-error "" } not suitable for overload
  (void)f;          // ok
  (void)ovl;        // { dg-error "" } not suitable for overload
  (void)&ovl;       // { dg-error "" } not suitable for overload
  static_cast<void>(f);          // ok
  static_cast<void>(ovl);        // { dg-error "" } not suitable for overload
  static_cast<void>(&ovl);       // { dg-error "" } not suitable for overload
  ((void)1, f);             // { dg-warning "" "" { xfail *-*-* } } not a call
  ((void)1, ovl);           // { dg-error "" } not suitable for overload
  ((void)1, &ovl);          // { dg-error "" } not suitable for overload
  (void)((void)1, f);           // ok
  (void)((void)1, ovl);         // { dg-error "" } not suitable for overload
  (void)((void)1, &ovl);        // { dg-error "" } not suitable for overload

  return 0;
}
