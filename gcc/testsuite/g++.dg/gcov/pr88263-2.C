// PR gcov-profile/88263
// { dg-options "-fprofile-arcs -ftest-coverage -std=c++11" }
// { dg-do run { target native } }
/* { dg-skip-if "requires hosted libstdc++ for sstream" { ! hostedlib } } */

#include <sstream>

namespace logging {
    class Logstream {
	~Logstream();
	static thread_local std::ostringstream os_;
    };
}
namespace logging {
    thread_local std::ostringstream Logstream::os_;
    Logstream::~Logstream() {
	os_.clear();
    }
}

int main()
{
  return 0;
}

// { dg-final { run-gcov pr88263-2.C } }
