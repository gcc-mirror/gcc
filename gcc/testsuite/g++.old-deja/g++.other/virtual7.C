// Build don't link:
// Special g++ Options: -fno-rtti
// Origin: Anthony Green <green@cygnus.com>

class _JvObjectPrefix
{
protected:
  virtual void finalize (void) = 0;
};

class Object : public _JvObjectPrefix
{
protected:
  virtual void finalize (void);
};

Object x;
