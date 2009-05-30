typedef unsigned int uint32_t;
static void IP(uint32_t v[2])
{
    v[0] = ((v[0] << 1) | ((v[0] >> 31) & 1L)) & 0xffffffffL;
}

