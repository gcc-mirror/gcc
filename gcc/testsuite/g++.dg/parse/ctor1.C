class L {
public:
  L(int);
};

class R {
  friend L::L(int);
};

