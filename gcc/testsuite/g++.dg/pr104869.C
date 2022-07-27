// PR rtl-optimization/104869
// { dg-do run }
// { dg-options "-O2 -fvisibility=hidden -std=c++11" }
// { dg-require-visibility "" }

struct QBasicAtomicInteger {
  [[gnu::noipa]] int loadRelaxed() { return 1; }
};
struct RefCount {
  bool deref() {
    int count = atomic.loadRelaxed();
    if (count)
      return false;
    return deref();
  }
  QBasicAtomicInteger atomic;
};
struct QArrayData {
  RefCount ref;
};
struct QString {
  ~QString();
  QArrayData d;
};
int ok;
QString::~QString() { d.ref.deref(); }
struct Label {
  bool isValid() { return generator; }
  int *generator;
  int index;
};
struct ControlFlow;
struct Codegen {
  [[gnu::noipa]] bool visit();
  ControlFlow *controlFlow;
};
struct ControlFlow {
  enum UnwindType { EE };
  struct UnwindTarget {
    Label linkLabel;
  };
  ControlFlow *parent;
  UnwindType unwindTarget_type;
  UnwindTarget unwindTarget() {
    QString label;
    ControlFlow *flow = this;
    while (flow) {
      Label l = getUnwindTarget(unwindTarget_type, label);
      if (l.isValid())
        return {l};
      flow = flow->parent;
    }
    return UnwindTarget();
  }
  [[gnu::noipa]] Label getUnwindTarget(UnwindType, QString &) {
    Label l = { &ok, 0 };
    return l;
  }
};
[[gnu::noipa]] void foo(int) {
  ok = 1;
}
[[gnu::noipa]] bool Codegen::visit() {
  if (!controlFlow)
    return false;
  ControlFlow::UnwindTarget target = controlFlow->unwindTarget();
  if (target.linkLabel.isValid())
    foo(2);
  return false;
}
int
main() {
  ControlFlow cf = { nullptr, ControlFlow::UnwindType::EE };
  Codegen c = { &cf };
  c.visit();
  if (!ok)
    __builtin_abort ();
}
