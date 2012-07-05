// PR c++/53848

extern "C"
{
  struct s {
    enum {
      e = 0
    } f;
  };
}
