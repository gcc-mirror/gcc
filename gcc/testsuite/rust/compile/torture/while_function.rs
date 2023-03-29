fn foo() {}
fn bar() -> i32 { return 10; }

fn main() {
	let mut i = 1;
	while i < bar() {
		foo();
		i += 1;
	}
}
