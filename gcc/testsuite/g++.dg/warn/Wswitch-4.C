// PR c++/105497
// { dg-do compile { target c++11 } }
// { dg-options "-Wswitch" }

enum class Button
{
    Left,
    Right,
    Middle,
    NumberOfButtons [[maybe_unused]]
};

enum class Sound
{
  Bark,
  Meow,
  Hiss,
  Moo __attribute((unused))
};

enum class Chordata
{
  Urochordata,
  Cephalochordata,
  Vertebrata
};

int main()
{
  Button b = Button::Left;
  switch (b) { // { dg-bogus "not handled" }
        case Button::Left:
        case Button::Right:
        case Button::Middle:
            break;
    }

  Sound s = Sound::Bark;
  switch (s) { // { dg-bogus "not handled" }
    case Sound::Bark:
    case Sound::Meow:
    case Sound::Hiss:
      break;
  }

  Chordata c = Chordata::Vertebrata;
  switch (c) { // { dg-warning "not handled" }
    case Chordata::Cephalochordata:
    case Chordata::Vertebrata:
      break;
  }
}
