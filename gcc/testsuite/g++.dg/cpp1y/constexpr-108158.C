// PR c++/108158
// { dg-do compile { target c++14 } }

template <class T, int N> struct carray {
  T data_[N]{};
  constexpr T operator[](long index) const { return data_[index]; }
};
struct seed_or_index {
private:
  long value_ = 0;
};
template <int M> struct pmh_tables {
  carray<seed_or_index, M> first_table_;
  template <typename KeyType, typename HasherType>
  constexpr void lookup(KeyType, HasherType) const {
    first_table_[0];
  }
};
template <int N> struct unordered_set {
  int equal_;
  carray<int, N> keys_;
  pmh_tables<N> tables_;
  constexpr unordered_set() : equal_{} {}
  template <class KeyType, class Hasher>
  constexpr auto lookup(KeyType key, Hasher hash) const {
    tables_.lookup(key, hash);
    return keys_;
  }
};
constexpr unordered_set<3> ze_set;
constexpr auto nocount = ze_set.lookup(4, int());
constexpr auto nocount2 = unordered_set<3>{}.lookup(4, int());
