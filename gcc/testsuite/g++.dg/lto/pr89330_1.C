typedef char gchar;
namespace Inkscape {
class Anchored {
int _anchor;
};
namespace XML {
enum NodeType {};
class Node :Anchored {
virtual NodeType type() ;
  virtual char const *name() const ;
  virtual int code() ;
  virtual unsigned position() ;
  virtual unsigned childCount() ;
  virtual char content() ;
  virtual char attribute() ;
  virtual int attributeList() ;
  virtual bool matchAttributeName() ;
  virtual void setPosition() ;
  virtual void setContent() ;
  virtual int document() ;
  virtual int document() const ;
  virtual Node *root() ;
  virtual Node *root() const ;
  virtual Node *parent() ;
  virtual Node *parent() const ;
  virtual Node *next() ;
  virtual Node const *next() const ;

};
class SimpleNode : virtual Node {
char const *name() const;
  Node *next() const { return _next; }
  SimpleNode *_next;
};
gchar const *SimpleNode::name() const { return 0; }
} } 
