// { dg-do compile }
// { dg-options "--std=c++11 -dA -gdwarf-4 -fdebug-types-section -fno-merge-debug-strings" }

// Check that -fdebug-types-sections does not copy a full referenced type
// into a type unit.

// Checks that at least one type unit is generated.
//
// { dg-final { scan-assembler "DIE \\(\[^\n\]*\\) DW_TAG_type_unit" } }
//
// Check that func is declared exactly once in the debug info (in the
// compile unit).
//
// { dg-final { scan-assembler-times "\\.ascii \"func\\\\0\"\[^\n\]*DW_AT_name" 1 } }
//
// Check to make sure that no type unit contains a DIE with DW_AT_low_pc
// or DW_AT_ranges.  These patterns assume that the compile unit is always
// emitted after all type units.
//
// { dg-final { scan-assembler-not "\\.quad\[^\n\]*DW_AT_low_pc.*DIE \\(\[^\n\]*\\) DW_TAG_compile_unit" } }
// { dg-final { scan-assembler-not "\\.quad\[^\n\]*DW_AT_ranges.*DIE \\(\[^\n\]*\\) DW_TAG_compile_unit" } }

struct A {
  A();
  virtual ~A();
  virtual void foo();
 private:
  int data;
};

struct B {
  B();
  virtual ~B();
};

extern B* table[];

struct D {
  template <typename T>
  T* get(int i)
  {
    B*& cell = table[i];
    if (cell == 0)
      cell = new T();
    return static_cast<T*>(cell);
  }
};

void func(D* d)
{
  struct C : B {
    A a;
  };
  d->get<C>(0)->a.foo();
}
