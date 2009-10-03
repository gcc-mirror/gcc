// { dg-lto-do link }
// { dg-lto-options {{-fPIC}} }
// { dg-extra-ld-options "-fPIC -shared" }

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

