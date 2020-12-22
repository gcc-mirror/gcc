// { dg-additional-options -fmodules-ts }

export module frob;
// { dg-module-cmi !frob }

int x ();
export int x (); // { dg-error "conflicting exporting declaration" }

int y;
export extern int y; // { dg-error "conflicting exporting declaration" }

typedef int z;
export typedef int z; // { dg-error "conflicting exporting declaration" }

template <typename T> int f (T);
export template <typename T> int f (T); // { dg-error "conflicting exporting declaration" }

// doesn't go via duplicate_decls so we miss this for now
class A;
export class A; // { dg-error "conflicting exporting declaration" "" { xfail *-*-* } }

// { dg-warning  "due to errors" "" { target *-*-* } 0 }
