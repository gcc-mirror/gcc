// { dg-additional-options -fmodules-ts }

export module frob;
// { dg-module-cmi !frob }

int x ();
export int x (); // { dg-error "conflicting exporting for declaration" }

int y;
export extern int y; // { dg-error "conflicting exporting for declaration" }

// A typedef is not an entity so the following is OK; see [module.interface] example 4
typedef int z;
export typedef int z; // { dg-bogus "conflicting exporting for declaration" }

template <typename T> using w = T;
export template <typename T> using w = T;  // { dg-error "conflicting exporting for declaration" }

template <typename T> int f (T);
export template <typename T> int f (T); // { dg-error "conflicting exporting for declaration" }

class A;
export class A; // { dg-error "conflicting exporting for declaration" }

template <typename T> struct B;
export template <typename T> struct B {};  // { dg-error "conflicting exporting for declaration" }
