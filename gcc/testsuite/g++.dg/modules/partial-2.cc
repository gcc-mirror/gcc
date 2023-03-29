static_assert(is_reference_v<int&>);
static_assert(is_reference_v<int&&>);
static_assert(!is_reference_v<int>);

static_assert(A::is_reference_v<long&>);
static_assert(A::is_reference_v<long&&>);
static_assert(!A::is_reference_v<long>);

#if __cpp_concepts
static_assert(concepts::is_reference_v<char&>);
static_assert(concepts::is_reference_v<char&&>);
static_assert(!concepts::is_reference_v<char>);

static_assert(concepts::A::is_reference_v<bool&>);
static_assert(concepts::A::is_reference_v<bool&&>);
static_assert(!concepts::A::is_reference_v<bool>);
#endif
