// PR c++/99895
// { dg-do compile { target c++20 } }

static constexpr unsigned hash(const char* s, unsigned length)
{
    s=s;
    return length;
}
template<unsigned N>
struct fixed_string
{
    constexpr fixed_string(const char (&s)[N])
    {
        for (int i = 0; i < N; i++)
            str[i] = s[i];
    }
    consteval const char* data() const { return str; }
    consteval unsigned size() const { return N-1; }
    char str[N];
};
template<unsigned expected_hash, fixed_string... s>
static consteval void VerifyHash()
{
    (
      [](auto){static_assert(hash(s.data(), s.size()) == expected_hash);}(s)
    ,...);
    // The compiler mistakenly translates s.data() into s.data(&s)
    // and then complains that the call is not valid, because
    // the function expects 0 parameters and 1 "was provided".
}
void foo()
{
    VerifyHash<5, "khaki", "plums">();
}
