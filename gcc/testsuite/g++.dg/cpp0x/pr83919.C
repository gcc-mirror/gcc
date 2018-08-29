// PR c++/83919
// { dg-do compile { target c++11 } }
// { dg-options "-Wignored-qualifiers" }

enum class Conf;
struct foo
{
  foo (const Conf& conf) : x{conf} {}	// { dg-bogus "type qualifiers ignored on cast result type" }
  const Conf x;
};
