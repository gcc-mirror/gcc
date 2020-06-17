/* PR c++/93804 - exempt extern "C" headers from -Wredundant-tags
   Verify that -Wredundant-tags is not issued for redundant class-key
   in extern "C" references in a header file.
   { dg-do compile }
   { dg-options "-Wredundant-tags -ftrack-macro-expansion=0" }  */

# 1 "Wredundant-tags-4.C"
# 1 "Wredundant-tags-4.h" 1
# line 10

#if __cplusplus >= 201103L
#  define enum_struct   enum struct
#else
#  define enum_struct   class
#endif

extern "C" {

  class C1 { };
  enum E1 { };
  enum_struct ES1 { };
  struct S1 { enum E1 e1; };
  union U1 { enum E1 e1; struct S1 s1; };

  /* The warning should be issued for the class-key class even in
     an extern "C" block.  */
  void f0 (class C1);                   // { dg-warning "\\\[-Wredundant-tags" }
  void f1 (enum E1);                    // { dg-bogus "\\\[-Wredundant-tags" }

  /* Ditto for a scoped enum.  */
  void f2 (enum_struct ES1);            // { dg-warning "\\\[-Wredundant-tags" }
                                        // { dg-warning "must not use the 'struct' keyword" "enum struct" { target { c++11 } } .-1 }

  void f3 (struct S1);                  // { dg-bogus "\\\[-Wredundant-tags" }
  void f4 (union U1);                   // { dg-bogus "\\\[-Wredundant-tags" }

  inline int
  finline1 (class C1)                   // { dg-warning "\\\[-Wredundant-tags" }
  { return sizeof (class C1); }         // { dg-warning "\\\[-Wredundant-tags" }

  inline int
  finline2 (enum E1)                    // { dg-bogus "\\\[-Wredundant-tags" }
  { return sizeof (enum E1); }          // { dg-bogus "\\\[-Wredundant-tags" }

  inline int
  finline3 (enum_struct ES1)            // { dg-warning "\\\[-Wredundant-tags" }
  { return sizeof (ES1); }

  inline int
  finline4 (struct S1)                  // { dg-bogus "\\\[-Wredundant-tags" }
  { return sizeof (struct S1); }

  inline int
  finline5 (union U1)                   // { dg-bogus "\\\[-Wredundant-tags" }
  { return sizeof (union U1); }

  extern class C1 c1;                   // { dg-warning "\\\[-Wredundant-tags" }
  extern enum E1 e1;                    // { dg-bogus "\\\[-Wredundant-tags" }
  extern enum_struct ES1 es1;           // { dg-warning "\\\[-Wredundant-tags" }
  extern struct S1 s1;                  // { dg-bogus "\\\[-Wredundant-tags" }
  extern union U1 u1;                   // { dg-bogus "\\\[-Wredundant-tags" }

  namespace N1 {
  /* Verify that -Wredundant-tags is issued in a namespace enclosed
     in an extern "C" block.  (Such code cannot be shared with C.)  */
  extern class C1 n1c1;                 // { dg-warning "\\\[-Wredundant-tags" }
  extern enum E1 n1e1;                  // { dg-warning "\\\[-Wredundant-tags" }
  extern enum_struct ES1 n1es1;         // { dg-warning "\\\[-Wredundant-tags" }
  extern struct S1 n1s1;                // { dg-warning "\\\[-Wredundant-tags" }
  extern union U1 n1u1;                 // { dg-warning "\\\[-Wredundant-tags" }
  }
}   // extern "C"


extern "C++" {

  class C2 { };
  enum E2 { };
  enum_struct ES2 { };
  struct S2 {
    enum E2 e21;                        // { dg-warning "\\\[-Wredundant-tags" }
    E2 e22;
    enum_struct ES2 es21;               // { dg-warning "\\\[-Wredundant-tags" }
    ES2 es22;
  };
  union U2 { };

  void f5 (class C2);                   // { dg-warning "\\\[-Wredundant-tags" }
  void f6 (enum E2);                    // { dg-warning "\\\[-Wredundant-tags" }
  void f7 (enum_struct ES2);            // { dg-warning "\\\[-Wredundant-tags" }
  void f8 (struct S2);                  // { dg-warning "\\\[-Wredundant-tags" }
  void f9 (union U2);                   // { dg-warning "\\\[-Wredundant-tags" }

  extern class C2 c2;                   // { dg-warning "\\\[-Wredundant-tags" }
  extern enum E2 e2;                    // { dg-warning "\\\[-Wredundant-tags" }
  extern enum_struct ES2 es2;           // { dg-warning "\\\[-Wredundant-tags" }
  extern struct S2 s2;                  // { dg-warning "\\\[-Wredundant-tags" }
  extern union U2 u2;                   // { dg-warning "\\\[-Wredundant-tags" }
}   // extern "C++"


namespace N {

class C3 { };
enum E3 { };
enum_struct ES3 { };
struct S3 { };
union U3 { };

void f10 (class C3);                    // { dg-warning "\\\[-Wredundant-tags" }
void f11 (enum E3);                     // { dg-warning "\\\[-Wredundant-tags" }
void f12 (enum_struct ES3);             // { dg-warning "\\\[-Wredundant-tags" }
void f13 (struct S3);                   // { dg-warning "\\\[-Wredundant-tags" }
void f14 (union U3);                    // { dg-warning "\\\[-Wredundant-tags" }

extern class C3 c3;                     // { dg-warning "\\\[-Wredundant-tags" }
extern enum E3 e3;                      // { dg-warning "\\\[-Wredundant-tags" }
extern enum_struct ES3 es3;             // { dg-warning "\\\[-Wredundant-tags" }
extern struct S3 s3;                    // { dg-warning "\\\[-Wredundant-tags" }
extern union U3 u3;                     // { dg-warning "\\\[-Wredundant-tags" }

extern "C" {

  /* Verify that -Wredundant-tags is issued in an extern "C" block
     enclosed within a namespace.  (Such code cannot be shared with
     C.)  */
  class C4 { };
  enum E4 { };
  enum_struct ES4 { };
  struct S4 { };
  union U4 { };

  extern class C4 c4;                   // { dg-warning "\\\[-Wredundant-tags" }
  extern enum E4 e4;                    // { dg-warning "\\\[-Wredundant-tags" }
  extern enum_struct ES4 es4;           // { dg-warning "\\\[-Wredundant-tags" }
  extern struct S4 s4;                  // { dg-warning "\\\[-Wredundant-tags" }
  extern union U4 u4;                   // { dg-warning "\\\[-Wredundant-tags" }
}

}   // namespace N

// { dg-prune-output "must not use the 'struct' keyword" }
