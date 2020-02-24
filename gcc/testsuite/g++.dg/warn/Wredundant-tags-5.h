#ifndef WREDUNDANT_TAGS_H
#define WREDUNDANT_TAGS_H

#if __cplusplus >= 201103L
# define enum_class   enum class
#else
# define enum_class   class
#endif

extern "C" {

  class C1 { };
  enum_class EC1 { };
  enum E1 { };
  struct S1 { };
  union U1 { };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-tags"
  class C1 fc1 (class C1);          // -Wredundant-tags
  enum_class EC1 fce1 (enum_class EC1);
#pragma GCC diagnostic pop

  enum E1 fe1 (enum E1);
  struct S1 fs1 (struct S1);
  union U1 fu1 (union U1);

  C1 fc1 (C1);
  EC1 fce1 (EC1);
  E1 fe1 (E1);
  S1 fs1 (S1);
  U1 fu1 (U1);
}


extern "C++" {

  class C2 { };
  enum_class EC2 { };
  enum E2 { };
  struct S2 { };
  union U2 { };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-tags"
  class C2 fc2 (class C2);                // -Wredundant-tags
  enum_class EC2 fce2 (enum_class EC2);   // -Wredundant-tags
  struct S2 fs2 (struct S2);              // -Wredundant-tags
  union U2 fu2 (union U2);                // -Wredundant-tags
#pragma GCC diagnostic pop

  C2 fc2 (C2);
  EC2 fce2 (EC2);
  E2 fe2 (E2);
  S2 fs2 (S2);
  U2 fu2 (U2);
}


class C3 { };
enum_class EC3 { };
enum E3 { };
struct S3 { };
union U3 { };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-tags"
class C3 fc3 (class C3);                  // -Wredundant-tags
enum_class EC3 fce3 (enum_class EC3);     // -Wredundant-tags
struct S3 fs3 (struct S3);                // -Wredundant-tags
union U3 fu3 (union U3);                  // -Wredundant-tags
#pragma GCC diagnostic pop

C3 fc3 (C3);
EC3 fce3 (EC3);
E3 fe3 (E3);
S3 fs3 (S3);
U3 fu3 (U3);

#endif   // WREDUNDANT_TAGS_H
