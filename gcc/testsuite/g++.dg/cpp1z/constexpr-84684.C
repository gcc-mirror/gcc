// PR c++/84684
// { dg-do compile { target c++17 } }

typedef decltype (sizeof (0)) size_t;

namespace std {
  template<class _E>
  struct initializer_list
  {
    typedef _E value_type;
    typedef const _E& reference;
    typedef const _E& const_reference;
    typedef size_t size_type;
    typedef const _E* iterator;
    typedef const _E* const_iterator;
    iterator _M_array;
    size_type _M_len;
    constexpr initializer_list(const_iterator __a, size_type __l) : _M_array(__a), _M_len(__l) { }
    constexpr initializer_list() noexcept : _M_array(0), _M_len(0) { }
    constexpr size_type size() const noexcept { return _M_len; }
    constexpr const_iterator begin() const noexcept { return _M_array; }
    constexpr const_iterator end() const noexcept { return begin() + size(); }
  };
}

template <typename E, size_t N>
struct array
{
  constexpr E &operator[](size_t n) noexcept { return elems[n]; }
  constexpr const E &operator[](size_t n) const noexcept { return elems[n]; }
  constexpr size_t size() const { return N; }
  E elems[N];
};

template<typename T>
constexpr
inline T
max (std::initializer_list<T> i)
{
  const T *b = i.begin ();
  const T *e = i.end ();
  if (b == e) return *b;
  const T *r = b;
  while (++b != e)
  if (*r < *b)
    r = b;
  return *r;
}

template <typename alphabet_type>
constexpr char to_char(alphabet_type const alph)
{
  return alph.to_char();
}

template <typename ...alphabet_types>
struct union_composition
{
  static constexpr size_t value_size = (alphabet_types::value_size + ... );
  unsigned char _value;
  template <size_t fixed_size, typename alphabet_t>
  static constexpr auto value_to_char_helper(alphabet_t alphabet)
  {
    array<char, fixed_size> value_to_char{};
    for (size_t i = 0u; i < alphabet_t::value_size; ++i)
      value_to_char[i] = to_char(alphabet.assign_rank(i));
    return value_to_char;
  }

  static constexpr auto make_value_to_char()
  {
    constexpr auto N = sizeof...(alphabet_types);
    constexpr array<size_t, N> alphabet_sizes { alphabet_types::value_size... };
    constexpr size_t fixed_size = max({alphabet_types::value_size...});
    array value_to_char_tables = array<array<char, fixed_size>, N> {
      value_to_char_helper<fixed_size>(alphabet_types{})...
    };
    array<char, value_size> value_to_char{};
    for (size_t i = 0u, value = 0u; i < N; ++i)
      for (size_t k = 0u; k < alphabet_sizes[i]; ++k, ++value)
        value_to_char[value] = value_to_char_tables[i][k];
    return value_to_char;
  }
};

struct gap
{
  constexpr char to_char() const noexcept { return '-'; }
  constexpr gap & assign_rank([[maybe_unused]] bool const i) noexcept { return *this; }
  static constexpr size_t value_size{1};
};

struct dna4
{
  constexpr char to_char() const noexcept { return value_to_char[_value]; }
  constexpr dna4 & assign_rank(unsigned char const c) { _value = c; return *this; }
  static constexpr size_t value_size{4};
  static constexpr char value_to_char[value_size] { 'A', 'C', 'G', 'T' };
  unsigned char _value;
};

struct dna5
{
  constexpr char to_char() const noexcept { return value_to_char[_value]; }
  constexpr dna5 & assign_rank(unsigned char const c) { _value = c; return *this; }
  static constexpr size_t value_size{5};
  static constexpr char value_to_char[value_size] { 'A', 'C', 'G', 'T', 'N' };
  unsigned char _value;
};

constexpr array value_to_char1 = union_composition<dna4>::make_value_to_char();
static_assert(value_to_char1.size() == 4u);
static_assert(value_to_char1[0] == 'A');
static_assert(value_to_char1[1] == 'C');
static_assert(value_to_char1[2] == 'G');
static_assert(value_to_char1[3] == 'T');

constexpr array value_to_char2 = union_composition<dna4, gap>::make_value_to_char();
static_assert(value_to_char2.size() == 5u);
static_assert(value_to_char2[0] == 'A');
static_assert(value_to_char2[1] == 'C');
static_assert(value_to_char2[2] == 'G');
static_assert(value_to_char2[3] == 'T');
static_assert(value_to_char2[4] == '-');

constexpr array value_to_char3 = union_composition<dna4, gap, dna5>::make_value_to_char();
static_assert(value_to_char3.size() == 10u);
static_assert(value_to_char3[0] == 'A');
static_assert(value_to_char3[1] == 'C');
static_assert(value_to_char3[2] == 'G');
static_assert(value_to_char3[3] == 'T');
static_assert(value_to_char3[4] == '-');
static_assert(value_to_char3[5] == 'A');
static_assert(value_to_char3[6] == 'C');
static_assert(value_to_char3[7] == 'G');
static_assert(value_to_char3[8] == 'T');
static_assert(value_to_char3[9] == 'N');

constexpr array value_to_char4 = union_composition<dna5, gap, dna4>::make_value_to_char();
static_assert(value_to_char4.size() == 10u);
static_assert(value_to_char4[0] == 'A');
static_assert(value_to_char4[1] == 'C');
static_assert(value_to_char4[2] == 'G');
static_assert(value_to_char4[3] == 'T');
static_assert(value_to_char4[4] == 'N');
static_assert(value_to_char4[5] == '-');
static_assert(value_to_char4[6] == 'A');
static_assert(value_to_char4[7] == 'C');
static_assert(value_to_char4[8] == 'G');
static_assert(value_to_char4[9] == 'T');

constexpr array value_to_char5 = union_composition<gap, dna4, dna5>::make_value_to_char();
static_assert(value_to_char5.size() == 10u);
static_assert(value_to_char5[0] == '-');
static_assert(value_to_char5[1] == 'A');
static_assert(value_to_char5[2] == 'C');
static_assert(value_to_char5[3] == 'G');
static_assert(value_to_char5[4] == 'T');
static_assert(value_to_char5[5] == 'A');
static_assert(value_to_char5[6] == 'C');
static_assert(value_to_char5[7] == 'G');
static_assert(value_to_char5[8] == 'T');
static_assert(value_to_char5[9] == 'N');
