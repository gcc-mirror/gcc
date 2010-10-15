// { dg-lto-do assemble }

class DOMString { };
class DocumentImpl;
class NodeImpl {
public:
    static const DOMString&
    mapPrefix(const DOMString &prefix, const DOMString &namespaceURI, short nType);
    static DOMString getXmlnsURIString();
};
class DOM_Node {
public:
    enum NodeType { ATTRIBUTE_NODE = 2 };
};
class AttrImpl: public NodeImpl {
public:
    AttrImpl(DocumentImpl *ownerDocument, const DOMString &aName);
};
class AttrNSImpl: public AttrImpl {
    AttrNSImpl(DocumentImpl *ownerDoc,  const DOMString &namespaceURI, const DOMString &qualifiedName);
};
AttrNSImpl::AttrNSImpl(DocumentImpl *ownerDoc,
		       const DOMString &fNamespaceURI,
		       const DOMString &qualifiedName)
  : AttrImpl(ownerDoc, qualifiedName)
{
    DOMString xmlnsURI = NodeImpl::getXmlnsURIString();
    DOMString prefix;
    bool xmlnsAlone = false;
    const DOMString& URI = xmlnsAlone ? xmlnsURI : mapPrefix(prefix, fNamespaceURI, DOM_Node::ATTRIBUTE_NODE);
}
