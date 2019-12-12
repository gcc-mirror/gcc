// PR c++/87137

// Empty macro args are undefined in C++ 98
// { dg-do compile { target c++11 } }

// We got confused by non-field decls separating bitfields when
// ms_bitfield_layout was in effect.

#if defined (__x86_64__) || defined (__i686__) || defined (__powerpc__)
#define ATTRIB  __attribute__((ms_struct))
#elif defined (__superh__)
#define ATTRIB __attribute__((renesas))
#else
#define ATTRIB
#endif

#define DEFINE(NAME,BASE,THING)		\
  struct ATTRIB NAME BASE {		\
    unsigned one : 12;			\
    THING				\
    unsigned two : 4;			\
  }

template<unsigned I, unsigned J> struct Test;
template<unsigned I> struct Test<I,I> {};

#define TEST(NAME,BASE,THING)			\
  DEFINE(NAME##_WITH,BASE,THING);		\
  DEFINE(NAME##_WITHOUT,BASE,);			\
  int NAME = sizeof (Test<sizeof(NAME##_WITH),sizeof (NAME##_WITHOUT)>)

TEST(NSFun,,int fun (););
TEST(SFun,,static int fun (););
TEST(Tdef,,typedef int tdef;);

TEST(Var,,static int var;);
struct base { int f; };
TEST(Use,:base,using base::f;);
TEST(Tmpl,,template<unsigned> class X {};);
TEST(TmplFN,,template<unsigned> void fu (););
