// Build don't link:
// Caused an Internal Compiler Error.  Works now.
// prms-id: 3070

class Object {
public:
  virtual ~Object() {}
};

class BaseView {
protected:
  virtual void _forwardReceiveUpdate() = 0;
};


class View : public BaseView {
public:
  virtual ~View();
};

class TestViewBaseUpdate {
protected:
  virtual void _receiveUpdate();
};

class TestViewBase : public TestViewBaseUpdate, private View {
};

class TestView : public Object, public TestViewBase {
protected:
  virtual void _receiveUpdate();
};
