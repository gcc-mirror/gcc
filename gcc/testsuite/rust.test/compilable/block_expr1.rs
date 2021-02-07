fn test3(x: i32) -> i32 {
    if x > 1 {
        5
    } else {
        0
    }
}

fn test5(x: i32) -> i32 {
    if x > 1 {
        if x == 5 {
            7
        } else {
            9
        }
    } else {
        0
    }
}

fn main() {
    let call3: i32 = { test3(3) + 2 };
    let call5 = {
        let a = test5(5);
        a + 1
    };
}
