// PR c++/98533
// { dg-do compile { target c++11 } }
// { dg-options "-g" }

class IR;
struct Pass {
  explicit Pass(IR *ir) : ir_(ir) {}
  virtual ~Pass() = default;
  IR *ir_ {nullptr};
};
struct PassManager {
  template <typename T> void RunPass() { T pass(ir_); }
  IR *ir_ {nullptr};
};
struct IR final {
  template <typename T> void RunPass() { pass_manager_.RunPass<T>(); }
  PassManager pass_manager_;
};
struct ThePass : Pass {
  explicit ThePass(IR *ir) : Pass(ir) {}
  ThePass(const ThePass &) = delete;
  template <typename Func = bool (*)(void *)> void Bar(void *inst, Func func = [](void *) {});
};

void foo(IR *ir) { ir->RunPass<ThePass>(); }
