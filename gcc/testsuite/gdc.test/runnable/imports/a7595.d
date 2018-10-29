@trusted:

import test7595;

struct Matrix
{
    int[4] _data;
}

void inverse(const ref Matrix m)
{
    for (size_t i = 0; i < 4; i++)
        assert(m._data[i] == i + 1);
}

void main()
{
    Matrix m9;
    m9._data[0] = 1;
    m9._data[1] = 2;
    m9._data[2] = 3;
    m9._data[3] = 4;
    for (size_t i = 0; i < 4; i++)
        assert(m9._data[i] == i + 1);

    benchmark!({ inverse(m9); })(1);
}
