//  PR C++/21645
//  We were crashing because we forgot to update the type for
//  the cloned argument for the cloned ctor.

struct color {
  ~color();
};
struct style {
  color col;
  style (color);
};

style::style(color c)
  : col(c)
{
}

