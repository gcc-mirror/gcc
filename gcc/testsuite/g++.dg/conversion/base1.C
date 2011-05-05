// PR c++/48749

struct Tuple3
{
  float x;
};

struct Pos: virtual Tuple3 { };

struct TexCoords
{
  Pos pos;
};

template <class T>
void eval (const TexCoords &coords)
{
  const Pos &pos = coords.pos;
  pos.x;
}
