// PR c++/100796
// { dg-additional-options "-Wsuggest-override -fplugin-arg-location_overflow_plugin-value=0x60000001" }
// Passing LINE_MAP_MAX_LOCATION_WITH_COLS meant we stopped distinguishing between lines in a macro.

#define DO_PRAGMA(text)           _Pragma(#text)
#define WARNING_PUSH              DO_PRAGMA(GCC diagnostic push)
#define WARNING_POP               DO_PRAGMA(GCC diagnostic pop)
#define WARNING_DISABLE(text)     DO_PRAGMA(GCC diagnostic ignored text)
#define NO_OVERRIDE_WARNING       WARNING_DISABLE("-Wsuggest-override")

#define BOILERPLATE				\
  WARNING_PUSH					\
  NO_OVERRIDE_WARNING				\
  void f();					\
  WARNING_POP

struct B
{
  virtual void f();
};

struct D: B
{
  BOILERPLATE
};
