// PR c++/13033

// We failed to treat the for increment expression as a full-expression,
// which broke gimplification.

struct QDomNode {
  virtual ~QDomNode();
  QDomNode nextSibling() const;
  bool isNull() const;
};

void processNode(QDomNode n)
{
  for (; !n.isNull(); n = n.nextSibling())
    ;
}
