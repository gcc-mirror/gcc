#include <stdexcept>
#if WORK_AROUND
typedef std::runtime_error std_runtime_error;
class X : public std_runtime_error {};
#else
class X : public std::runtime_error {};
#endif
