// PR gcov-profile/88263
// { dg-options "-fprofile-arcs -ftest-coverage -std=c++11" }
// { dg-do run { target native } }

#include <sstream>

namespace log {

class Logstream {
public:

private:
    /// The logging stream
    static thread_local std::ostringstream os_;
};

}

namespace log {

thread_local std::ostringstream Logstream::os_;

}

int main()
{
  return 0;
}

// { dg-final { run-gcov pr88263.C } }
