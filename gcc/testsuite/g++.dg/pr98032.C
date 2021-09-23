// { dg-do compile }

namespace osl {
class Thread {
public:
  virtual ~Thread();
  virtual void join();
};
} // namespace osl
class SimpleReferenceObject {
protected:
  virtual ~SimpleReferenceObject();
};
class Thread : SimpleReferenceObject, osl::Thread {
public:
  using osl::Thread::join;
};
class RenderThread : Thread {
  RenderThread() { join(); }
};
