/* PR c++/94078 - bogus and missing -Wmismatched-tags on an instance
   of a template
   Verify that -Wmismatched-tags is issued for redeclarations and
   instances of the appropriate primary template or specialization.
  { dg-do compile }
  { dg-options "-Wmismatched-tags" } */

// Exercise explicit specialization.
template <class> class S1;
template <>      struct S1<int>;

template <class> class S1;
template <class> struct S1;           // { dg-warning "\\\[-Wmismatched-tags" }

template <>      class S1<char>;
template <>      struct S1<char>;     // { dg-warning "\\\[-Wmismatched-tags" }

template <>      class S1<int>;       // { dg-warning "\\\[-Wmismatched-tags" }
template <>      struct S1<int>;

extern        S1<void> s1v;
extern class  S1<void> s1v;
extern struct S1<void> s1v;           // { dg-warning "\\\[-Wmismatched-tags" }

extern        S1<int> s1i;
extern class  S1<int> s1i;            // { dg-warning "\\\[-Wmismatched-tags" }
extern struct S1<int> s1i;

extern        S1<char> s1c;
extern class  S1<char> s1c;
extern struct S1<char> s1c;           // { dg-warning "\\\[-Wmismatched-tags" }


// Exercise partial specialization.
template <class>   struct S2;
template <class T> class S2<const T>;

template <class>   class S2;          // { dg-warning "\\\[-Wmismatched-tags" }
template <class>   struct S2;

template <class T> class S2<const T>;
template <class T> struct S2<const T>;// { dg-warning "\\\[-Wmismatched-tags" }

extern        S2<int> s2i;
extern class  S2<int> s2i;            // { dg-warning "\\\[-Wmismatched-tags" }
extern struct S2<int> s2i;

extern        S2<const int> s2ci;
extern class  S2<const int> s2ci;
extern struct S2<const int> s2ci;     // { dg-warning "\\\[-Wmismatched-tags" }


template <class>   struct S3;
template <class T> class S3<T*>;
template <class T> struct S3<T&>;

template <class>   class S3;          // { dg-warning "\\\[-Wmismatched-tags" }
template <class>   struct S3;

template <class T> class S3<T*>;
template <class T> struct S3<T*>;     // { dg-warning "\\\[-Wmismatched-tags" }

template <class T> class S3<T&>;      // { dg-warning "\\\[-Wmismatched-tags" }
template <class T> struct S3<T&>;

extern        S3<int> s3i;
extern class  S3<int> s3i;            // { dg-warning "\\\[-Wmismatched-tags" }
extern struct S3<int> s3i;

extern        S3<int*> s3p;
extern class  S3<int*> s3p;
extern struct S3<int*> s3p;           // { dg-warning "\\\[-Wmismatched-tags" }

extern        S3<int&> s3r;
extern class  S3<int&> s3r;           // { dg-warning "\\\[-Wmismatched-tags" }
extern struct S3<int&> s3r;

// Repeat exactly the same as above.
extern        S3<int> s3i;
extern class  S3<int> s3i;            // { dg-warning "\\\[-Wmismatched-tags" }
extern struct S3<int> s3i;

extern        S3<int*> s3p;
extern class  S3<int*> s3p;
extern struct S3<int*> s3p;           // { dg-warning "\\\[-Wmismatched-tags" }

extern        S3<int&> s3r;
extern class  S3<int&> s3r;           // { dg-warning "\\\[-Wmismatched-tags" }
extern struct S3<int&> s3r;

// Repeat the same as above just with different type.
extern        S3<long> s3l;
extern class  S3<long> s3l;           // { dg-warning "\\\[-Wmismatched-tags" }
extern struct S3<long> s3l;

extern        S3<long*> s3lp;
extern class  S3<long*> s3lp;
extern struct S3<long*> s3lp;         // { dg-warning "\\\[-Wmismatched-tags" }

extern        S3<long&> s3lr;
extern class  S3<long&> s3lr;         // { dg-warning "\\\[-Wmismatched-tags" }
extern struct S3<long&> s3lr;

// Repeat with the class-keys swapped.
extern        S3<long> s3l;
extern struct S3<long> s3l;
extern class  S3<long> s3l;          // { dg-warning "\\\[-Wmismatched-tags" }

extern        S3<long*> s3lp;
extern struct S3<long*> s3lp;        // { dg-warning "\\\[-Wmismatched-tags" }
extern class  S3<long*> s3lp;

extern        S3<long&> s3lr;
extern struct S3<long&> s3lr;
extern class  S3<long&> s3lr;        // { dg-warning "\\\[-Wmismatched-tags" }


namespace N
{
template <class> struct A;

extern class A<int> ai;               // { dg-warning "\\\[-Wmismatched-tags" }
extern struct A<int> ai;

typedef class A<int> AI;              // { dg-warning "\\\[-Wmismatched-tags" }
typedef struct A<int> AI;

template <class> struct B;
template <> class B<int>;
template <> struct B<char>;

extern class B<int> bi;
extern struct B<int> bi;              // { dg-warning "\\\[-Wmismatched-tags" }

extern class B<char> bc;              // { dg-warning "\\\[-Wmismatched-tags" }
extern struct B<char> bc;

typedef class B<char> BC;             // { dg-warning "\\\[-Wmismatched-tags" }
typedef struct B<char> BC;

}
