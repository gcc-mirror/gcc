struct s { char p[2]; };
static struct s v;
const int o0 = (int) ((void *) &v.p[0] - (void *) &v) + 0;
const int o1 = (int) ((void *) &v.p[0] - (void *) &v) + 1;
