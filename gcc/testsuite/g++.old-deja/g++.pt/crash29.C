// { dg-do assemble  }
// Origin: Steven Parkes <parkes@sierravista.com>

typedef __SIZE_TYPE__ size_t;

class UUId {};

template <class T> class MetaClass;

class TypeInfo;

struct MetaClassGeneric 
{
                                MetaClassGeneric( TypeInfo& );
};

struct TypeInfo 
{
                        void    (*constructor)( void* );
                        void    initialize( void* );
};

template <class T>
class TypeIDInit {
public:
                                TypeIDInit();
                 static void    initialize();
             static TypeInfo    info;
                  static int    storage[];
                 static void    metaclassConstructor( void* );
};

template <class T>
TypeInfo TypeIDInit<T>::info = 
{
  TypeIDInit<T>::metaclassConstructor
};

template <class T>
inline
TypeIDInit<T>::TypeIDInit()
{
  info.initialize(storage);
}

template <class T>
class NameInfo : public MetaClassGeneric {
public:
                                NameInfo() 
				: MetaClassGeneric( TypeIDInit<T>::info ) {}
};

template <>
class MetaClass<UUId>
: public NameInfo<UUId>
{
};

extern "C++"
inline void *operator new(size_t, void *place) throw() { return place; }

template <class T>
void
TypeIDInit<T>::metaclassConstructor( void* place )
{
  new ( place ) MetaClass<T>;
}

template class   TypeIDInit<UUId>   ;

