template<bool _Const>
struct _Iterator
{
private:
  static void mover (const _Iterator &arg = {}) noexcept (noexcept (arg));
    
public:
  _Iterator() = default;

  friend void move (const _Iterator &arg2) noexcept (noexcept (mover (arg2)))
  {}
};
