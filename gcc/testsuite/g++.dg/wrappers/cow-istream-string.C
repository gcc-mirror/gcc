template<typename _CharT>
struct basic_string {
  static const _CharT _S_terminal;
  static void assign(const _CharT& __c2);
  void _M_set_length_and_sharable() {
    assign(_S_terminal);
  }
};

template<typename _CharT>
const _CharT basic_string<_CharT>::_S_terminal = _CharT();

void getline(basic_string<char>& __str) {
  __str._M_set_length_and_sharable();
}
