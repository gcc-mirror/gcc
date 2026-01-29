// { dg-do compile }
ulong pure_hashOf(const ref typeof(*null) key)
{
    return hashOf(key);
}

ulong hashOf(const typeof(*null) val)
{
    return 0;
}
