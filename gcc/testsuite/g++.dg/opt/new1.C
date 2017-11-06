// PR c++/39367
// { dg-options "-O" }

class QScriptEnginePrivate;
class QScriptClassInfo;
namespace QScript {
    enum Type { InvalidType };
};
class QScriptValueImpl {
public:
    inline QScriptValueImpl();
    QScript::Type m_type;
};
namespace QScript {
    namespace Ecma {
        class Core {
        public:
            inline QScriptEnginePrivate *engine() const     { return 0; }
            inline QScriptClassInfo *classInfo() const     { return 0; }
            QScriptValueImpl publicPrototype;
        };
        class Boolean: public Core {
            void newBoolean(QScriptValueImpl *result, bool value = false);
        };
    }
    template <typename T> class Buffer     {
    public:
        inline void reserve(int num);
        inline void resize(int s);
        T *m_data;
        int m_capacity;
        int m_size;
    };
}
template <typename T> void QScript::Buffer<T>::resize(int s) {
    if (m_capacity < s)
      reserve (s << 1);
}
template <typename T> void QScript::Buffer<T>::reserve(int x) {
    T *new_data = new T[m_capacity];
    for (int i=0; i<m_size; ++i)
      new_data[i] = m_data[i];
}
class QScriptObject {
public:
    inline void reset();
    QScript::Buffer<QScriptValueImpl> m_values;
};
class QScriptEnginePrivate {
public:
  inline QScriptObject *allocObject() { return 0; }
    inline void newObject(QScriptValueImpl *o, const QScriptValueImpl &proto,
                          QScriptClassInfo *oc = 0);
};
inline void QScriptEnginePrivate::newObject(QScriptValueImpl *o,
                                            const QScriptValueImpl &proto,
                                            QScriptClassInfo *oc)
{
  QScriptObject *od = allocObject();
  od->reset();
}
inline QScriptValueImpl::QScriptValueImpl() : m_type(QScript::InvalidType) { }
inline void QScriptObject::reset() { m_values.resize(0); }
namespace QScript {
    namespace Ecma {
        void Boolean::newBoolean(QScriptValueImpl *result, bool value)
          {
            engine()->newObject(result, publicPrototype, classInfo());
          }
    }
}
