// PR c++/103338
// { dg-do compile { target c++11 } }

template<class...>
struct zip_view {
  struct Iterator;
};

template<class...>
struct zip_transform_view;

template<class... Views>
struct zip_view<Views...>::Iterator { // { dg-error "no class template" }
  template<class... Uiews>
  template<bool>
  friend class zip_transform_view<Uiews...>::Iterator;
};

zip_view<>::Iterator iter;
