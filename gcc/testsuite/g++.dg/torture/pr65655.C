/* { dg-do compile } */
// { dg-timeout-factor 2.0 }
// { dg-additional-options "-std=c++11 -fsanitize=undefined -O2 -Wno-return-type" }
class ECoordinate { };
class EPoint {
public:
  inline ECoordinate & y ();
};
ECoordinate & EPoint::y () { }
template < class KEY, class CONTENT > class AVLTree;
template < class KEY, class CONTENT > class AVLTreeNode {
  friend class
    AVLTree < KEY, CONTENT >;
  KEY key;
  void set_rthread (unsigned char b);
  void set_lthread (unsigned char b);
};
template < class KEY, class CONTENT > class AVLTree {
public:
  AVLTree ();
  void insert (const KEY & key, const CONTENT & c);
AVLTreeNode < KEY, CONTENT > *root;
  const KEY * _target_key;
  virtual int compare (const KEY & k1, const KEY & k2) const;
  void _add (AVLTreeNode < KEY, CONTENT > *&t);
  virtual void _status (unsigned int) { }
};
template < class KEY, class CONTENT > void AVLTree < KEY, CONTENT >::_add (AVLTreeNode < KEY, CONTENT > *&t) {
  int cmp = compare (*_target_key, t->key);
  if (cmp == 0)
    { _status (1); }
}
template < class KEY, class CONTENT > void AVLTree < KEY, CONTENT >::insert (const KEY & key, const CONTENT & c) {
  if (root == 0) {
      root->set_rthread (1);
      root->set_lthread (1);
    }
else { _target_key = &key; _add (root); }
}
template < class KEY, class CONTENT > AVLTree < KEY, CONTENT >::AVLTree ()
: root (0) { }
class ContactRepository {
  void insertContact (EPoint & pt, int val);
};
void ContactRepository::insertContact (EPoint & pt, int val) {
  AVLTreeNode < ECoordinate, AVLTree < ECoordinate, int >*>*cont_x_node;
  if (cont_x_node == __null)
    {
      AVLTree < ECoordinate, int >*insert_tree = new AVLTree < ECoordinate, int >;
      insert_tree->insert (pt.y (), val);
    }
}
