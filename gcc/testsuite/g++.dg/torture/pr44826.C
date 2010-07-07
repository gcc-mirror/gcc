typedef unsigned short PRUint16;
typedef PRUint16 PRUnichar;
template <class CharT> struct nsCharTraits {
};
class nsAString_internal   {
public:
    typedef PRUnichar char_type;
};
class nsString : public nsAString_internal   {
public:
    typedef nsString self_type;
    nsString( const self_type& str );
};
class nsDependentString : public nsString   {
public:
    explicit nsDependentString( const char_type* data );
};
typedef struct sqlite3_stmt sqlite3_stmt;
const void *sqlite3_column_text16(sqlite3_stmt*, int iCol);
class nsIVariant { };
template <typename DataType> struct variant_storage_traits {
    typedef DataType ConstructorType;
    typedef DataType StorageType;
    static inline StorageType storage_conversion(ConstructorType aData) {
        return aData;
    }
};
template <typename DataType> class Variant : public nsIVariant {
public:
    Variant(typename variant_storage_traits<DataType>::ConstructorType aData)
        : mData(variant_storage_traits<DataType>::storage_conversion(aData)) {}
    typename variant_storage_traits<DataType>::StorageType mData;
};
typedef Variant<nsString> TextVariant;
class Row {
    void initialize(sqlite3_stmt *aStatement);
};
void Row::initialize(sqlite3_stmt *aStatement)
{
  nsDependentString str(static_cast<const PRUnichar
*>(::sqlite3_column_text16(aStatement, 0)));
  new TextVariant(str);
}

