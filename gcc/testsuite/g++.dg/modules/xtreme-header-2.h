// Everything that transitively includes <ranges>

// FIXME: There remains a defect in c++20 handling, tickled by ranges
#if __cplusplus <= 201703
#include <algorithm>
#include <execution>
#include <functional>
#include <future>
#include <memory>
#include <memory_resource>
#include <regex>
#include <scoped_allocator>
#include <thread>
#include <valarray>
#if __cplusplus > 201703
#include <ranges>
#endif
#endif
