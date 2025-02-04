// PR c++/117114 - Reduced version of another bootstrap error
// Unrelated methods can have the same DECL_VINDEX when the class hierarchy
// depth is 2 or more.
// { dg-do compile { target c++11 } }
// { dg-additional-options -Woverloaded-virtual }

class HIRFullVisitor;
class HIRTypeVisitor;

struct FullVisitable {
  virtual void accept_vis (HIRFullVisitor &vis) = 0;
};

struct Node {
  virtual ~Node() {}
};

struct Type : Node, FullVisitable
{
  using FullVisitable::accept_vis;
  virtual void accept_vis (HIRTypeVisitor &vis) = 0;
};

struct TypePath : Type
{
  void accept_vis (HIRFullVisitor &vis) override;
  void accept_vis (HIRTypeVisitor &vis) override;
};
