// { dg-lto-do link }
// { dg-lto-options {{-fPIC}} }
// { dg-extra-ld-options "-fPIC -shared" }

class DataArray {
    int max() const    { }
};
template < class HashItem > 
class DataHashTable {
    template < class ElemHashItem >
	class Element    { };
    typedef Element< HashItem > Elem;
    DataArray m_elem;
};
class Name    { };
class NameSet {
    DataHashTable < Name > hashtab;
};
NameSet p;

