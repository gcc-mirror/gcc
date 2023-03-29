// PR c++/31747

static extern int i; // { dg-error "'extern' specifier conflicts with 'static'" }
