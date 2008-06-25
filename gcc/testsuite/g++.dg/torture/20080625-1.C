extern "C" {
    typedef unsigned char sal_Bool;
    typedef struct _rtl_uString {
    } rtl_uString;
    void rtl_uString_release( rtl_uString * str ) throw ();
}
class OUString {
    rtl_uString * pData;
public:
    OUString() {}
    ~OUString() {
	rtl_uString_release( pData );
    }
    sal_Bool equalsIgnoreAsciiCase( const OUString & str ) const;
};
bool findAndRemove();
long getAttributeProps()
{
    long nAttrs = 0;
    OUString aValue;
    if (findAndRemove()
	&& aValue.equalsIgnoreAsciiCase(OUString()))
	;
    else 
        nAttrs |= 1;
    return nAttrs;
}
