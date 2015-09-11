/* pr52773.c */

struct s {
    short x;
    short _pad[2];
};

static short mat_a_x;

void transform(const struct s *src, struct s *dst, int n)
{
    int i;

    for (i = 0; i < n; ++i)
	dst[i].x = (src[i].x * mat_a_x) >> 6;
}
