// Bug: g++ tries to generate an op= for DbmItem and fails.
// Build don't link:

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
