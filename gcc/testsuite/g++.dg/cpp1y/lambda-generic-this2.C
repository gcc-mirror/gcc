// PR c++/85764
// { dg-do compile { target c++14 } }

template<typename Key>
class trie {
    static void for_each(int & f, trie const & n, Key & prefix) {
        [&](trie const & c) {
          for_each(f, c, prefix);
        };
    }
    void for_each(int & f) const {
    }
};
