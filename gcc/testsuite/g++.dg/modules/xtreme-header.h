// All the headers!

#if __cplusplus > 201703L
// FIXME: if we include everything, something goes wrong with location
// information.  We used to not handle lambdas attached to global
// vars, and this is a convienient flag to stop including everything.
#define NO_ASSOCIATED_LAMBDA 1
#endif

// C++ 17 and below
#if 1
#if !NO_ASSOCIATED_LAMBDA
#include <algorithm>
#endif
#include <any>
#include <array>
#include <atomic>
#include <bitset>
#include <charconv>
#include <chrono>
#include <complex>
#include <condition_variable>
#include <cctype>
#include <cuchar>
#include <cwchar>
#include <cwctype>
#include <deque>
#include <exception>
#if !NO_ASSOCIATED_LAMBDA
// FIXME: PR 97549
//#include <execution>
#endif
#include <filesystem>
#include <forward_list>
#include <fstream>
#if !NO_ASSOCIATED_LAMBDA
#include <functional>
#endif
#if !NO_ASSOCIATED_LAMBDA
#include <future>
#endif
#include <initializer_list>
#include <iomanip>
#include <ios>
#include <iosfwd>
#include <istream>
#include <iterator>
#include <limits>
#include <list>
#include <locale>
#include <map>
#if !NO_ASSOCIATED_LAMBDA
#include <memory>
#endif
#if !NO_ASSOCIATED_LAMBDA
#include <memory_resource>
#endif
#include <mutex>
#include <new>
#include <numeric>
#include <optional>
#include <ostream>
#include <queue>
#include <random>
#include <ratio>
#if !NO_ASSOCIATED_LAMBDA
#include <regex>
#endif
#if !NO_ASSOCIATED_LAMBDA
#include <scoped_allocator>
#endif
#include <set>
#include <shared_mutex>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <streambuf>
#include <string>
#include <string_view>
#include <system_error>
#if !NO_ASSOCIATED_LAMBDA
#include <thread>
#endif
#include <tuple>
#include <type_traits>
#include <typeindex>
#include <typeinfo>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#if !NO_ASSOCIATED_LAMBDA
#include <valarray>
#endif
#include <variant>
#include <vector>
#endif

// C compatibility
#if 1
#include <cassert>
#include <cerrno>
#include <cfenv>
#include <cfloat>
#include <cinttypes>
#include <climits>
#include <clocale>
#include <cmath>
#include <csetjmp>
#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#endif

// C++20
#if __cplusplus > 201703
#if 1
#include <version>
#include <bit>
#include <compare>
#include <concepts>
#if __cpp_coroutines
#include <coroutine>
#endif
#if !NO_ASSOCIATED_LAMBDA
#include <ranges>
#endif
#include <numbers>
#include <span>
#include <stop_token>
#if 0
// Unimplemented
#include <barrier>
#include <format>
#include <latch>
#include <semaphore>
#include <source_location>
#include <syncstream>
#endif
#endif
#endif
