// { dg-options "" }

struct a {
        int x;
};

struct b {
        int x;
        int y;
};

struct foo {
        union {
                struct a a;
                struct b b;
        } u;
};

int main(void)
{
        struct foo bar = { u: { b: { x: 0, y: 0, }}};
        (void)bar;
        return 0;
}
