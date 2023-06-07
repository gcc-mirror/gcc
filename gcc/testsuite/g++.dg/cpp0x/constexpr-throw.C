// { dg-do compile { target c++11 } }
// Explicit { dg-require-effective-target exceptions_enabled } to avoid verify compiler messages FAILs for '-fno-exceptions'.

constexpr int may_throw(bool decide) {
	return decide ? 42 : throw -1; // { dg-error "throw" }
}

constexpr int x = may_throw(false); // { dg-message "may_throw" }
constexpr int y = may_throw(true);
