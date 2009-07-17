/* { dg-do compile } */
/* { dg-options "-O" } */

class ios_base   {
public:
    virtual ~ios_base();
};
template<typename _CharT>
class basic_ostream : virtual public ios_base {
public:
    virtual ~basic_ostream() { }
};
extern template class basic_ostream<char>;
template <typename _CharT>
class basic_ostringstream : public basic_ostream<_CharT> { };
template class basic_ostringstream<char>;
