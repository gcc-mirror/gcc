// PR c++/92852
// { dg-do compile { target c++14 } }

struct S { int operator<<(const int &); } glob;
void foo()
{
  S& message_stream = glob;
  auto format = [&message_stream](auto && x)
		{ message_stream << x ; };
  format(3);
  format(4u);
}
