// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_noexcept.  Negative cases.

#include <meta>
#include <functional>

// Lambdas
const auto noexcept_lambda = []() noexcept {};
const auto not_noexcept_lambda = []{};

static_assert (!std::meta::is_noexcept (^^noexcept_lambda));
static_assert (!std::meta::is_noexcept (^^not_noexcept_lambda));

// Template lamdas
const auto noexcept_template_lambda = []<typename T>(T arg) noexcept {};
const auto not_noexcept_template_lambda = []<typename T>(T arg) {};

static_assert (!std::meta::is_noexcept (^^noexcept_template_lambda));
static_assert (!std::meta::is_noexcept (^^not_noexcept_template_lambda));

void noexcept_function () noexcept;
void not_noexcept_function ();

// Function pointers
void (*noexcept_function_pointer)() noexcept = noexcept_function;
void (*not_noexcept_function_pointer)() = not_noexcept_function;

static_assert (!std::meta::is_noexcept (^^noexcept_function_pointer));
static_assert (!std::meta::is_noexcept (^^not_noexcept_function_pointer));

// Function reference
void (&noexcept_function_reference)() noexcept = noexcept_function;
void (&not_noexcept_function_reference)() = not_noexcept_function;

static_assert (!std::meta::is_noexcept (^^noexcept_function_reference));
static_assert (!std::meta::is_noexcept (^^not_noexcept_function_reference));

struct S {
};

// Member function pointers
void (S::*noexcept_member_pointer)() noexcept;
void (S::*not_noexcept_member_pointer)();

static_assert (!std::meta::is_noexcept (^^noexcept_member_pointer));
static_assert (!std::meta::is_noexcept (^^not_noexcept_member_pointer));

// Function pointer as a parameter in function
void function_with_noexcept_function_pointer_as_input(void (*noexcept_function_pointer)() noexcept) {
  static_assert (!std::meta::is_noexcept (^^noexcept_function_pointer));
}

void function_with_not_noexcept_function_pointer_as_input(void (*not_noexcept_function_pointer)()) {
  static_assert (!std::meta::is_noexcept (^^not_noexcept_function_pointer));
}

// Misc
std::function<void(int)> f;
int n = 42;

static_assert (!std::meta::is_noexcept (^^f));
static_assert (!std::meta::is_noexcept (^^n));
static_assert (!std::meta::is_noexcept (^^S));
static_assert (!std::meta::is_noexcept (^^::));
