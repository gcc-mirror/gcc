// P0847R7
// { dg-do run { target c++23 } }

// an adaptation of one of the examples in P0847R7

struct Leaf { };
struct Node;

struct Tree {
  enum class stored {leaf, node};
  stored _discriminator;
  union {
    Leaf _leaf;
    Node* _node;
  };
  Tree(Leaf) : _discriminator(stored::leaf), _leaf() {}
  Tree(Node& node) : _discriminator(stored::node), _node(&node) {}
};

struct Node {
    Tree left;
    Tree right;
};

template<typename Visitor>
auto visit_tree(Visitor&& visitor, Tree const& tree)
{
  switch (tree._discriminator)
  {
    case Tree::stored::leaf:
      return visitor (tree._leaf);
    case Tree::stored::node:
      return visitor (tree._node);
    default:
      __builtin_abort (); 
  }
}

template<typename... Ts>
struct overload : Ts... { using Ts::operator()...; };

int main()
{
  static constexpr int true_num_leaves = 8;
  Node branch0{.left = Leaf{}, .right = Leaf{}};
  Node branch1{.left = Leaf{}, .right = branch0};
  Node branch2{.left = Leaf{}, .right = Leaf{}};
  Node branch3{.left = branch1, .right = branch2};
  Node branch4{.left = branch3, .right = Leaf{}};
  Node branch5{.left = Leaf{}, .right = Leaf{}};
  Node branch6{.left = branch4, .right = branch5};

  Tree root (branch6);

  int num_leaves = visit_tree (overload{
    [](Leaf const&) { return 1; },
    [](this auto const& self, Node* n) -> int {
      return visit_tree (self, n->left) + visit_tree (self, n->right);
    }},
    root);
  if (num_leaves != true_num_leaves)
    __builtin_abort ();
}

