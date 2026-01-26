/* { dg-do run } */
/* PR tree-optimization/123820 */

struct Trit { char value; };

struct Matrix {
    int width;
    Trit* data;

    Trit& at(int x, int y) {
        if (!(x >= y)) __builtin_abort ();
        return data[y * width];
    }
};


Trit set_value;
[[gnu::used]]
int EmbedPositionDetectionPattern_yStart;

[[gnu::used,gnu::noipa]]
void EmbedPositionDetectionPattern(Matrix& m) {
    for (int y = 0; y < 7; ++y)
        for (int x = 0; x < 7; ++x)
            m.at(x, EmbedPositionDetectionPattern_yStart + y) = {0};

    for (int i = 1; i < 8; ++i) {
        if (i < m.width)
            m.at(i, 7) = {0};

        int y = EmbedPositionDetectionPattern_yStart + i;
        if (7 < m.width && y)
            m.at(7, y) = {0};
    }
}

int main()
{
  return 0;
}

