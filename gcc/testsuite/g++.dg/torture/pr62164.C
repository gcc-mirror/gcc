// { dg-do compile }
// { dg-require-effective-target named_sections }

class T { static void t(); };

class U
{
public:
  static void u() __attribute__ ((__section__ (".initcall.text")));
};

inline void U::u() {}

void T::t() { U::u(); }
