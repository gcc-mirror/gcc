struct First {
    i: usize,
}

impl First {
    pub fn e_function(mut self) {
        self.i += 1;
    }
}

pub fn main() {
    let a = (First { i: 10 }, 2);
    a.0.e_function();
}
