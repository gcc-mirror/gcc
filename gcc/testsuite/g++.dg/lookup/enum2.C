// PR c++/14476

struct tree_common {
  enum tree_code code : 8;      // { dg-error "" }
};
