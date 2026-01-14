// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test from P3491R3 3.5.5
// https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2025/p3491r3.html#implementing-source_location

#include <meta>

class source_location {
    struct impl {
	char const* filename;
	int line;
    };
    impl const* p_;
    constexpr source_location(impl const* x) noexcept : p_{x} {}

public:
    static consteval auto current(char const* file = __builtin_FILE(),
				  int line = __builtin_LINE()) noexcept
	-> source_location
    {
	// first, we canonicalize the file
	// Note, the paper uses just define_static_string(file), but that
	// doesn't work, char const* argument isn't a valid input_range.
	impl data = {.filename = std::define_static_string(std::string_view(file)), .line = line};

	// then we canonicalize the data
	impl const* p = std::define_static_object(data);

	// and now we have an external linkage object mangled with this location
	return source_location{p};
    }
    constexpr source_location() noexcept : p_{nullptr} { }
    constexpr int line() const noexcept { return p_ ? p_->line : 0; }
    constexpr const char* file_name() const noexcept { return p_ ? p_->filename : ""; }
};

static_assert (::source_location::current ().line() == std::source_location::current ().line ());
static_assert (::source_location::current ().line() == std::source_location::current ().line ());
static_assert (::source_location::current ().line()
	       == std::source_location::current ().line () - 1);
static_assert (std::string_view (::source_location::current ().file_name ())
	       == std::string_view (std::source_location::current ().file_name ()));
static_assert (::source_location().line() == 0);
static_assert (std::string_view (::source_location ().file_name ())
	       == std::string_view (""));
