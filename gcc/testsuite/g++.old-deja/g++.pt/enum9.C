// Build don't link:

template <typename _CharT>
class _Format_cache
{
public:  
  enum {   
    _S_digits,   _S_digits_end = _S_digits+10,
    _S_xdigits = _S_digits_end
  };
};

template class _Format_cache<int>;
