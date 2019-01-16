// EXTRA_SOURCES: imports/a15333.d

module ice15333;

void map(alias fun)() {}

struct IdentifierResolver(alias handler)
{
    void resolve()
    {
        map!((a) {});
        handler(true);
    }
}
