// { dg-options -std=c++0x }

constexpr int may_throw(bool decide) {
	return decide ? 42 : throw -1; // { dg-error "throw" }
}

constexpr int x = may_throw(false); // { dg-message "may_throw" }
constexpr int y = may_throw(true);
