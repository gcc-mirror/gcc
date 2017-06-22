// { dg-do compile }
// { dg-additional-options "-mcpu=970 -maltivec" { target powerpc*-*-* } }

typedef union {
    long int asBits;
} jsval_layout;
static jsval_layout STRING_TO_JSVAL_IMPL() {}

typedef __attribute__ ((aligned(sizeof (long int)))) long int jsval;
class Value {
public:
    void setString() {
	data = STRING_TO_JSVAL_IMPL();
    }
    jsval_layout data;
} __attribute__ ((aligned(8)));

static Value StringValue()
{
  Value v;
  v.setString();
  return v;
}

static const jsval & Jsvalify(const Value & v)
{
  return (const jsval &)v;
}

static Value *Valueify(jsval *v)
{
  return (Value *) v;
}

struct JSObject {
    void getQNameLocalName();
};
static Value IdToValue(int id)
{
  if (id)
    return StringValue();
}

static jsval IdToJsval(int id)
{
  return Jsvalify(IdToValue(id));
}

class AutoGCRooter;
struct JSContext {
    AutoGCRooter *autoGCRooters;
};
class AutoGCRooter {
public:
    AutoGCRooter(JSContext *cx) {}
};
class AutoArrayRooter:AutoGCRooter {
public:
    AutoArrayRooter(JSContext *cx, Value *vec):AutoGCRooter(cx)
    {
      array = vec;
      cx->autoGCRooters = this;
    }
    Value *array;
};

static void PutProperty(JSContext *cx, int id, jsval *vp)
{
  JSObject *nameobj;
  jsval roots[3];
  roots[1] = IdToJsval(id);
  roots[2] = *vp;
  AutoArrayRooter tvr(cx, Valueify(roots));
  nameobj->getQNameLocalName();
}

void xml_defineProperty(JSContext *cx, int id, const Value *v)
{
  jsval tmp = Jsvalify(*v);
  PutProperty(cx, id, &tmp);
}
