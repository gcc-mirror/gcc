// PR c++/118904

#include <source_location>
inline std::source_location foo() {
  return std::source_location::current();
}
