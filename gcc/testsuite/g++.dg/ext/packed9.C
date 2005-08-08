// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Apr 2005 <nathan@codesourcery.com>

// DR21166. unnecessary error on packed char

struct s1 {
        char    c1;
} __attribute__((packed));

char&
f(struct s1 *s)
{
        return s->c1;
}

char *
g(struct s1 *s)
{
        return &s->c1;
}
