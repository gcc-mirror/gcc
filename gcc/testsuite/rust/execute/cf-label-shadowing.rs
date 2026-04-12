// { dg-output "100\r*\n200\r*\n300\r*\n201\r*\n302\r*\n101\r*\n210\r*\n310\r*\n211\r*\n312\r*\n102\r*\n220\r*\n320\r*\n221\r*\n322\r*\n999" }
#![feature(no_core)]
#![no_core]
extern "C" {
    fn printf(s: *const i8, ...);
}
fn dump_number(num: i32) {
    unsafe {
        let fmt = "%i\n\0";
        let c = fmt as *const str as *const i8;
        printf(c, num);
    }
}

fn main() -> i32 {
    let mut outer = 0;

    'l: while outer < 3 {
        dump_number(100 + outer);

        let mut middle = 0;
        'l: loop {
            dump_number(200 + outer * 10 + middle);

            let mut inner = 0;
            'l: loop {
                dump_number(300 + outer * 10 + middle * 2 + inner);
                break 'l;
            }

            middle += 1;
            if middle >= 2 {
                break 'l;
            }
        }

        outer += 1;
        if outer < 3 {
            continue 'l;
        }

        break 'l;
    }

    dump_number(999);
    0
}
