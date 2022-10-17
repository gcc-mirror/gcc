struct Field(int _w, int _h)
{
    bool[_h][_w] s;
}

struct Life(int w, int h)
{
    auto a = new Field!(w, h); // ICE
}

alias T = Life!(100, 100);
