//Build don't link:
//Based on a report by Helmut Jarausch <jarausch@IGPM.Rwth-Aachen.DE>
template<class>
class foo{};

namespace ABC
{
  using ::foo;
}
