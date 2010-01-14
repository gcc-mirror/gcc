struct QVectorData {
    static QVectorData shared_null;
};
template <typename T> class QVector {
    union {
        QVectorData *d;
    };
public:
    inline QVector() : d(&QVectorData::shared_null) { }
    inline QVector(const QVector<T> &v) : d(v.d) { }
};
class QXmlStreamAttribute { };
class QXmlStreamAttributes : public QVector<QXmlStreamAttribute> { };
class __attribute__ ((visibility("default"))) Smoke {
public:
    union StackItem;
    typedef StackItem* Stack;
    typedef short Index;
};
class SmokeBinding { };
namespace __smokeqt {
    class x_QXmlStreamAttributes : public QXmlStreamAttributes {
        SmokeBinding* _binding;
    public:
        static void x_11(Smoke::Stack x) {
            x_QXmlStreamAttributes* xret = new x_QXmlStreamAttributes();
        }
        explicit x_QXmlStreamAttributes() : QXmlStreamAttributes() { }
    };
    void xcall_QXmlStreamAttributes(Smoke::Index xi, void *obj,
                                    Smoke::Stack args)
      {
        switch(xi) {
            case 11: x_QXmlStreamAttributes::x_11(args);
        }
      }
}
