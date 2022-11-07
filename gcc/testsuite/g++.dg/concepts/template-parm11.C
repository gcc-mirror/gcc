// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<typename T>
concept bool NameProvider()
{
  return requires(){
    typename T::_name_t::template _member_t<int>;
  };
}

template<NameProvider... ColSpec>
void getTable(const ColSpec&...)
{}

void f()
{
  getTable(7, 'a'); // { dg-error "" }
};
