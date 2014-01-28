// PR c++/58639
// { dg-require-effective-target c++11 }

struct node {
  node &parent;
};

struct vector {
  node n;
};

vector v({});			// { dg-error "" }
