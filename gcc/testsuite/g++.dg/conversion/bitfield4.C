// PR c++/27292

enum ColorRole
{
  WindowText, Button
};

struct QPalette {

ColorRole bg_role:8;

 bool hasBackground();
};


bool
QPalette::hasBackground ()
{
  return (ColorRole (bg_role) != WindowText);
}
