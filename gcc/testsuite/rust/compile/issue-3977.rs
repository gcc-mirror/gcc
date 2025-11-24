// Test for issue #3977 - ICE with continue/break/return in while condition

fn diverge() -> ! {
    loop {}
}

fn test_continue() {
    loop {
        while continue {}
    }
}

fn test_break() {      
    loop {
        while break {}
    }
}

fn test_return() {
    loop {
        while return {}
    }
}

fn test_labeled_break() {
    'outer: loop {
        loop {
            while break 'outer {}
        }
    }
}

fn test_labeled_continue() {
    'outer: loop {
        loop {
            while continue 'outer {}
        }
    }
}

fn test_complex_if_else() {
    loop {
        while if true { continue } else { break } {}
    }
}

fn foo() {
    while diverge() {
        break
    }
    let _x = 5;
}

fn main() {
    // Just reference them so they're "used"
    if false {
        test_continue();
        test_break();
        test_return();
        test_labeled_break();
        test_labeled_continue();
        test_complex_if_else();
        foo();
    }
}