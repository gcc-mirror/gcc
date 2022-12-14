fn foo() {}


fn main() {
    let _c = *{
	let _a = foo;
	let b = &1;
	b
    };
}