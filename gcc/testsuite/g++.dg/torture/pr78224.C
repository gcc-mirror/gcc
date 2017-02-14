// { dg-do compile }

extern "C"{
  float sqrtf(float);
}

inline float squareroot(const float f)
{
  return sqrtf(f);
}

inline int squareroot(const int f)
{
  return static_cast<int>(sqrtf(static_cast<float>(f)));
}

template <class T>
class vector2d
{
public:
  vector2d(T nx, T ny) : X(nx), Y(ny) {}
  T getLength() const { return squareroot( X*X + Y*Y ); }
  T X;
  T Y;
};

vector2d<int> getMousePos();

class Client
{
public:
  Client();
  ~Client();
};

void the_game(float turn_amount)
{
  Client client;
  bool first = true;

  while (1) {
      if (first) {
        first = false;
      } else {
        int dx = getMousePos().X;
        int dy = getMousePos().Y;

        turn_amount = vector2d<float>(dx, dy).getLength();
      }
  }
}
