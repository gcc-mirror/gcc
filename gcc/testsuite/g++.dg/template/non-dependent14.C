// PR c++/64251

class DictionaryValue {};
template <typename T> void CreateValue(T) {
  DictionaryValue(0);
  CreateValue(0);
}
