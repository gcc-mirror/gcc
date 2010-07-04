// { dg-do compile }
// { dg-options "-O2 -Warray-bounds" }

void f();

int c[3];
int result;

struct Vector {
    static int get(int i) {
        if (i >= 3)
            f();
        return c[i];
    }
};

void g()
{
    for (int i = 0; i < 3; ++i) {
        const int index = i % 3;
        result = Vector::get(index) + Vector::get(index);
    }
}

