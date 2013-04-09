// PR c++/25466
// { dg-do run }

#include <typeinfo>

const std::type_info *a;

template <class T>
bool is_polymorphic() {
   bool result(false);
   const std::type_info &a1 = typeid( (result=true), *(T*)0);
   a = &a1;
   return result;
}

struct non_polymorphic {};
struct polymorphic { virtual ~polymorphic() {} };


int main() {
  if (is_polymorphic<int>()) __builtin_abort();
  if (is_polymorphic<non_polymorphic>()) __builtin_abort();
  try
    {
      is_polymorphic<polymorphic>();
      __builtin_abort(); // should have thrown bad_typeid
    }
  catch (std::bad_typeid&)
    {
      // OK
    }
  catch (...)
    {
      __builtin_abort();
    }
}
