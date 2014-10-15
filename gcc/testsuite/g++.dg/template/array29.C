// PR c++/63485

template <typename C> struct A
{
  typedef C type;
};
template <class> class B
{
};
template <class Range> void as_literal (Range &);
template <typename> struct C
{
  typedef wchar_t char_type;
  const char_type on_full_year_placeholder[3];
  void
  on_extended_iso_date ()
  {
    B<A<wchar_t const[3]>::type> a;
    as_literal (on_full_year_placeholder);
  }
};
template <typename> struct date_time_format_parser_callback : C<wchar_t>
{
};
template <typename BaseT> struct D
{
  typedef typename BaseT::char_type char_type;
  char_type
  parse (const char_type *, const char_type *,
         typename BaseT::callback_type p3)
  {
    p3.on_extended_iso_date ();
  }
};
struct F
{
  typedef date_time_format_parser_callback<wchar_t> callback_type;
  typedef wchar_t char_type;
};
template <typename CharT, typename ParserT, typename CallbackT>
void
parse_format (CharT *p1, ParserT p2, CallbackT p3)
{
  CharT p = p2.parse (&p, p1, p3);
}
template <typename CharT>
void
parse_date_time_format (const CharT *, const CharT *p2,
                        date_time_format_parser_callback<CharT> &p3)
{
  D<F> b;
  parse_format (p2, b, p3);
}
template void
parse_date_time_format (const wchar_t *, const wchar_t *,
                        date_time_format_parser_callback<wchar_t> &);
