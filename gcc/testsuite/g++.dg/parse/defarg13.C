// PR c++/38485

template <class Key, class T>
class QMap { };

class XMLConfigurations {
  void translateToOther(QMap<int, int> match = (QMap<int, int>()));
};
