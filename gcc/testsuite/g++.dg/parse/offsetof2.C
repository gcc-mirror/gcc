#include <cstddef>

struct choke_me
{
    int size;
    char storage[1];
};

struct offset_is_broken
{
    static const int offset = offsetof(choke_me, storage);
};
