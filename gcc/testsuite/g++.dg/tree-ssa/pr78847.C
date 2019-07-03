/* { dg-do compile } */
/* { dg-require-effective-target c++14 } */
/* { dg-options "-O3 -fdump-tree-ldist-optimized" } */

#include <stddef.h>
#include <cstring>
#include <experimental/string_view>

using string_view = std::experimental::string_view;

class Foo {
    constexpr static size_t Length = 9;
    char ascii_[Length];
public:
    Foo();
    string_view view() const {
	return string_view(ascii_, Length);
    }
};

void testWithLoopValue(const Foo foo, size_t ptr, char *buf_) {
    for (auto c : foo.view())
      buf_[ptr++] = c;
}

/* { dg-final { scan-tree-dump "split to 0 loops and 1 library calls" "ldist" } } */
