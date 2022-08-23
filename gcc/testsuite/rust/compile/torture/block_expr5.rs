fn foo() -> i32 {
    0
}

fn bar() -> i32 {
    foo();
    foo()
}

fn baz() -> i32 {
    {
        bar();
        bar();
    }
    {
        bar();
        bar()
    };
    {
        bar();
        bar()
    }
}

fn test(ok: i32) -> i32 {
    if ok >= 1 {
        foo()
    } else if ok <= -1 {
        bar()
    } else {
        baz()
    }
}

fn main() {
    let a = foo();
    let b = bar();
    let c = baz();
    test(a + b + c);
}
