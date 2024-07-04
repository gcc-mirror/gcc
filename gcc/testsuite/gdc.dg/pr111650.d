// { dg-do compile }
ref V require(K, V)(ref V[K] aa, K key, lazy V value);

struct Root
{
    ulong[3] f;
}

Root[ulong] roots;

Root getRoot(int fd, ulong rootID)
{
    return roots.require(rootID,
    {
            Root result;
            inoLookup(fd, () => result);
            return result;
    }());
}

void inoLookup(int, scope Root delegate()) { }
