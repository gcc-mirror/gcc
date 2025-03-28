pub struct AA {
    pub data: [u8; 10],
}

impl AA {
    pub const fn new() -> Self {
        let mut res: AA = AA { data: [0; 10] };
        res.data[0] = 5;
        res
    }
}

static mut BB: AA = AA::new();

fn main() {
    let _ptr = unsafe { &mut BB };
}
