// { dg-lto-do link }
// { dg-lto-options { { -O3 -g -flto -shared -fPIC -Wno-odr } } }
// { dg-require-effective-target fpic }
// { dg-require-effective-target shared }

namespace Inkscape {
class Anchored {};
namespace XML {
enum NodeType {};
class Node :Anchored {
public:
  virtual NodeType type() ;
  virtual char name() ;
  virtual int code() ;
  virtual unsigned position() ;
  virtual unsigned childCount() ;
  virtual char content() ;
  virtual char *attribute() const ;
  virtual int attributeList() ;
  virtual bool matchAttributeName() ;
  virtual void setPosition() ;
  virtual void setContent() ;
  virtual void setAttribute() ;
  virtual int document() ;
  virtual int document() const ;
  virtual Node *root() ;
  virtual Node *root() const ;
  virtual Node *parent() ;
  virtual Node *next() const ;
  virtual Node *firstChild() const ;

};
} } struct rdf_license_t {
  };
;
class RDFImpl {
;
  rdf_license_t *getLicense();
};
static bool rdf_match_license(Inkscape::XML::Node const *repr) {
  for (Inkscape::XML::Node *current = repr->firstChild(); current;
       current->next()->attribute());
  return 0;
}
rdf_license_t *RDFImpl::getLicense() {
  Inkscape::XML::Node *repr ;
  for (rdf_license_t *license ; license;
       license) {
    rdf_match_license(repr);
  }
  return 0;
}
