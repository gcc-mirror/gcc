// PR c++/54441
// { dg-options "" }

struct s { char c[]; };

int main()
{
    struct s s = { .c = 0 };	// { dg-error "initializer" }
    return 0;
}
