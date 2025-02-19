// PR c++/94100
// { dg-do compile { target c++11 } }

template <typename... T> struct ValListWithTypes {
  template <T... Members> struct WithVals {
    using TypeList = ValListWithTypes;
  };
};

template <typename ValList, typename ValTypeList = typename ValList::TypeList>
struct Widget;

template <typename ValList, typename... ValTypes>
struct Widget<ValList, ValListWithTypes<ValTypes...>> {
  template <typename = ValList> struct Impl {};
};

template <typename ValList, typename... ValTypes>
template <ValTypes... Vals>
struct Widget<ValList, ValListWithTypes<ValTypes...>>::Impl<
    typename ValListWithTypes<ValTypes...>::template WithVals<Vals...>> {};

int main(void) { Widget<ValListWithTypes<int>::WithVals<0>>::Impl<> impl; }
