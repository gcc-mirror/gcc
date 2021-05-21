// PR c++/100205
// { dg-do compile { target c++11 } }

struct coordinate_matrix {
  using index_t = unsigned;
  struct convert_to_matrix_coordinate {
    index_t column_id;
  };
  index_t column_id;

  // does not work
  using value_type2 = decltype(convert_to_matrix_coordinate{column_id});

  // does work
  using value_type5 = decltype(column_id);
};
