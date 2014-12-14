// { dg-options "-Wabi-tag" }

inline namespace A __attribute ((abi_tag)) {
  struct Foo { };		// { dg-message "declared here" }
  struct Baz: Foo { };
}

struct Bar: Foo { };		// { dg-warning "tag" }
