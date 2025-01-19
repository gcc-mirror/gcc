// { dg-xfail-if "PR108277" { arm_thumb1 } }

struct Error {
  virtual void error(... ) const;
};

struct ChildNode : virtual Error {
  void error(... ) const;
};

void ext(const char*, ...);

void ChildNode::error(...) const
{
#ifdef FIX
  ext("");
#endif
}
