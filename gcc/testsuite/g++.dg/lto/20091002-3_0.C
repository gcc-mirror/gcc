// { dg-lto-do link }
// { dg-require-effective-target fpic }
// { dg-lto-options {{-fPIC}} }
// { dg-extra-ld-options "-fPIC -r -nostdlib -flinker-output=nolto-rel" }

template < class T > 
class DataArray {
    int max() const { return 0; }
};
class Name { };
class DataHashTable {
    template < class ElemHashItem > class Element { };
    DataArray < Element < Name > > m_elem;
};
DataHashTable p;

