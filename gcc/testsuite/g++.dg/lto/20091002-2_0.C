// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-lto-options {{-fPIC}} }
// { dg-extra-ld-options "-fPIC -r -nostdlib" }

class DataArray {
    int max() const    { return 0; }
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

