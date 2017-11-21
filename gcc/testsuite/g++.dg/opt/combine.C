// { dg-do assemble { target fpic } }
// { dg-options "-O2 -fweb -fPIC -fvisibility=hidden -Wno-return-type" }
// { dg-require-visibility "" }

class QBasicAtomicInt
{
public:
  volatile int _q_value;
  inline operator int () const {return _q_value;}
};
class QVariant;
class QScriptContext;
class QScriptEngine;
class QScriptValue
{
public:
  QVariant toVariant () const;
};
class QScriptDebuggerBackendPrivate
{
  static QScriptValue trace (QScriptContext *context);
};
template <typename T> struct QMetaTypeId { };
template <typename T> struct QMetaTypeId2
{
  static inline int qt_metatype_id ()
  {
    return QMetaTypeId<T>::qt_metatype_id () ;
  }
};
template <typename T> inline int qMetaTypeId (T * = 0)
{
  return QMetaTypeId2<T>::qt_metatype_id () ;
}
class QVariant { };
template<typename T> inline T qvariant_cast (const QVariant &v)
{
  const int vid = qMetaTypeId<T> ((0)) ;
  return T();
};
class QScriptContext
{
public: 
  QScriptValue callee () const;
};
class QScriptEngine  
{
public:
  static bool convertV2 (const QScriptValue &value , int type , void *ptr) ;
};
inline bool qscriptvalue_cast_helper (const QScriptValue &value , int type , void *ptr)
{
  return QScriptEngine::convertV2 (value, type, ptr) ;
}
template<typename T> T qscriptvalue_cast (const QScriptValue &value)
{
  T t;
  const int id = qMetaTypeId<T> () ;
  if ( qscriptvalue_cast_helper (value, id, &t))
    return qvariant_cast<T> (value.toVariant ()) ;
}
template <> struct QMetaTypeId< QScriptDebuggerBackendPrivate* >
{
  static int qt_metatype_id ()
  {
    static QBasicAtomicInt metatype_id = { (0) };
    return metatype_id;
  }
};
QScriptValue QScriptDebuggerBackendPrivate::trace (QScriptContext *context)
{
  QScriptValue data = context->callee () ;
  QScriptDebuggerBackendPrivate *self = qscriptvalue_cast<QScriptDebuggerBackendPrivate*> (data) ;
  return QScriptValue();
}
