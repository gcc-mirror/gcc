// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-lto-options {{-fPIC}} }
// { dg-extra-ld-options "-fPIC -r -nostdlib" }

template < class T > 
class DataArray {
    int max() const { }
};
class Name { };
class DataHashTable {
    template < class ElemHashItem > class Element { };
    DataArray < Element < Name > > m_elem;
};
DataHashTable p;

