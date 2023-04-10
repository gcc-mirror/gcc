template fullyQualifiedName(T...)
{
    enum fullyQualifiedName = !T[0];
}

void __trace_maybeDumpTupleToFile(Args...)(auto ref const Args args) nothrow @nogc { }

int getStructInfoEx(T)() {
   enum Ctx = fullyQualifiedName!T;
   return 0;
}

auto as(Func)(Func) {}

@nogc void foo() { }

void assertOp(string OPERATION, LHS, RHS)(LHS lhs, RHS) {
  as({
    try {
      try as(lhs);
      catch(Throwable) foo();
    } catch(Throwable) assert(false);
  });
}

struct FixedArray(T, size_t capacity_) {
  int a = getStructInfoEx!FixedArray;

  T* some_function() {
    assertOp !""(1, 1);
    return null;
  }
  alias some_function this;
}

struct ReclamationBatch {

  FixedArray !(uint,1) dummy;

  @nogc nothrow void some_inout_func() inout { }

  void func_2(Dlg)(Dlg dlg) {
    __trace_maybeDumpTupleToFile(dlg);
  }

  void _reclaimBatch() {
    func_2({ some_inout_func; });
  }
}
