/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-all" } */

class DocumentImpl;
struct NodeImpl
{
  virtual DocumentImpl * getOwnerDocument();
  virtual NodeImpl * getParentNode();
  virtual NodeImpl * removeChild(NodeImpl *oldChild);
};
struct AttrImpl : NodeImpl
{
  NodeImpl *insertBefore(NodeImpl *newChild, NodeImpl *refChild);
};
struct DocumentImpl : NodeImpl
{
  virtual NodeImpl *removeChild(NodeImpl *oldChild);
  virtual int* getRanges();
};
NodeImpl *AttrImpl::insertBefore(NodeImpl *newChild, NodeImpl *refChild) {
  NodeImpl *oldparent = newChild->getParentNode();
  oldparent->removeChild(newChild);
  this->getOwnerDocument()->getRanges();
  return 0;
}
