struct SS {
    one: i32,
    two: i32,
}
struct TS(i32, i32);

fn main() {
    unsafe {
        let ss = SS { one: 1, two: 2 };
        let _ts = TS(ss.one, ss.two);
    };
}
