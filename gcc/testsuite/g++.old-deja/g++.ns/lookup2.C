// Build don't link: 
template <class charT>
struct basic_string
{
  charT append (charT c)
    { return c; }
};
typedef char c;
template class basic_string <char>;
