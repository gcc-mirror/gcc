// PR c++/39367 - ICE at tree-inline.c:1042 with -O
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
    /* The following may be optimized into a trap because the function
       is called from resize(0) and so with m_capacity < 0.  When not
       optimized it may trigger -Walloc-size-larger-than= since
       operator new() is called with an excessively large value.
       The warning is pruned from the test output below.  */
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

// { dg-prune-output "\\\[-Walloc-size-larger-than=]" }
