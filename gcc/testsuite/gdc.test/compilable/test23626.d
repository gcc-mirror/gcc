// https://issues.dlang.org/show_bug.cgi?id=23626
// EXTRA_SOURCES: extra-files/test23626a.d extra-files/test23626b.d
module test23626;

struct StaticHashTable(V)
{
    V v;
}
