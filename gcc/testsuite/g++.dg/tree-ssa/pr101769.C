// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O2 -fdump-tree-optimized" }

struct Node
{
  Node*	right;
  Node*	down;
};

inline
void free_node(Node*)
{
}

void free_all(Node* n_)
{
  if (n_ == nullptr) {
      return;
  }
  free_all(n_->right);
  do {
      Node* t = n_->down;
      free_node(n_);
      n_ = t;
  } while (n_);
}

void free_all2_r(Node* n_)
{
  if (n_->right) {
      free_all2_r(n_->right);
  }
  do {
      Node* t = n_->down;
      free_node(n_);
      n_ = t;
  } while (n_);
}

void free_all2(Node* n_)
{
  if (n_) {
      free_all2_r(n_);
  }
}

void loop(Node* n_)
{
  do {
      n_ = n_->down;
  } while (n_);
}

// All functions should be empty.
// { dg-final { scan-tree-dump-times "<bb " 4 "optimized" } }
