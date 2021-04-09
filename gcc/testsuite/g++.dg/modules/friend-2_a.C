// { dg-additional-options -fmodules-ts }

export module bink;
// { dg-module-cmi bink }

class pusher 
{
  friend void frob (pusher *){}
public:
  pusher (){}
};

inline void grabber (pusher *p)
{
  frob (p);
}
