// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100967
// { dg-do compile }

module object;

class Object {}
class TypeInfo {}
class TypeInfo_AssociativeArray : TypeInfo {} // { dg-note "defined here" }

extern(C) int main()
{
    int[int] aa; // { dg-error "no property" }
    aa[0] = 1;  // { dg-error "'object._d_aaGetY' not found" }
    return 0;
}
