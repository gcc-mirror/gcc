// PR c++/119807
// { dg-do compile { target c++20 } }

using size_t = decltype(sizeof(0));

template<auto tag, size_t current>
struct CounterReader {
	template<typename>
	friend auto counterFlag(CounterReader<tag, current>) noexcept;
};

template<auto tag, size_t current>
struct CounterWriter {
	static constexpr size_t value = current;

	template<typename>
	friend auto counterFlag(CounterReader<tag, current>) noexcept {}
};

template<auto tag, auto unique, size_t current = 0, size_t mask = size_t(1) << (sizeof(size_t) * 8 - 1)>
[[nodiscard]] constexpr size_t counterAdvance() noexcept {
	if constexpr (!mask) {
		return CounterWriter<tag, current + 1>::value;
	} else if constexpr (requires { counterFlag<void>(CounterReader<tag, current | mask>()); }) {
		return counterAdvance<tag, unique, current | mask, (mask >> 1)>();
	} 
	else {
		return counterAdvance<tag, unique, current, (mask >> 1)>();
	}
}

constexpr auto defaultCounterTag = [] {};

template<auto tag = defaultCounterTag, auto unique = [] {}>
constexpr size_t counter() noexcept {
	return counterAdvance<tag, unique>();
}

int main() {
	static_assert(counter() == 1);
	static_assert(counter() == 2);
}
