// PR c++/5104
// Test that a function with a throw spec is a valid template argument.

#include <exception>

typedef void (*HandlerFunction)();
typedef HandlerFunction (*SetHandlerFunction)(HandlerFunction);

template <SetHandlerFunction set_function>
class HandlerStack {
public:
  static void defaultHandler();
};

typedef HandlerStack<std::set_terminate> Terminate;

template<> void Terminate::defaultHandler() {};
