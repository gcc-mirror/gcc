// { dg-do assemble  }
// Bug: g++ tries to generate an op= for DbmItem and fails.

class RefCount{
public:
  RefCount();

private:
  RefCount& operator=(const RefCount);
};

class DbmItem: public RefCount{
public:
  DbmItem(): RefCount() {};
};
