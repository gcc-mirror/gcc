// PR c++/19808
// { dg-do compile }
// { dg-options "-Wuninitialized" }

class diagnostic_event {
public:
  virtual int get_stack_depth();
};
struct event_range {
  event_range(diagnostic_event &initial_event)
      : m_stack_depth(initial_event.get_stack_depth()) {}
  int m_stack_depth;
};
