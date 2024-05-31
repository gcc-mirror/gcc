// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
concept NameProvider =
  requires(){
    typename T::_name_t::template _member_t<int>;
  };

template<NameProvider... ColSpec>
void getTable(const ColSpec&...)
{}

void f()
{
  getTable(7, 'a'); // { dg-error "" }
};
